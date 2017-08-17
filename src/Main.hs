{-# LANGUAGE LambdaCase, NoMonomorphismRestriction, OverloadedStrings, ScopedTypeVariables #-}

module Main where

import Prelude

import Control.Arrow ((>>>))
import Control.Lens
import Data.Aeson.Types as AT
import Data.Aeson.Lens (AsValue, _Number, _String, key, values)
import Data.Bitraversable (bitraverse)
import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Internal as BSI
import qualified Data.List as List
import Data.Monoid ((<>))
import Data.Monoid.Endo (E)
import qualified Data.Maybe as Maybe
import Data.Map.Lazy (Map)
import qualified Data.Map.Lazy as Map
import Data.Text.Lens (_Text)
import qualified Network.Wreq as Wreq


-- Edit These

-- Available Ticker Symbols
data TickerSymbol
  = SCHB
  | SCHF
  | SCHE
  | SCHD
  | TFI
  | BWX
  | SCHP
  | GII
  | PSAU
  | GLTR
  | CGW
  deriving (Bounded, Enum, Eq, Ord, Read, Show)

-- Schwab Free-Trade ETFs @ Lower Risk
-- http://www.schwab.com/public/schwab/investing/investment_help/investment_research/etf_research/etfs.html?&path=/Prospect/Research/etfs/overview/oneSourceETFs.asp
holdings :: [Holding]
holdings =
  [ Holding SCHB 16 0.27  -- US Broad
  , Holding SCHF 15 0.15  -- Foreign Developed
  , Holding SCHE 12 0.11  -- Emerging
  , Holding SCHD  6 0.09  -- US Dividend
  , Holding TFI  12 0.19  -- US Municipal Bond
  , Holding BWX   6 0.06  -- Intl. Treasury Bond
  , Holding SCHP  2 0.04  -- TIPS
  , Holding GII   2 0.05  -- Global Infrastructure
  , Holding PSAU  5 0.02  -- Mining
  , Holding GLTR  0 0.01  -- Precious Metals
  , Holding CGW   0 0.01  -- Water
  ]

-- Symbol, Current Shares, Percent Target
data Holding =
  Holding TickerSymbol Int Rational
  deriving (Eq)

-- TODO: yeah
instance Show Holding where
  show (Holding s h t) =
    show s <> " : " <> show h <> " - " <> show (fromIntegral (round $ t * 1000) / 10) <> "%"

toStockPriceTuple :: AsValue t => t -> Maybe ( TickerSymbol, Rational )
toStockPriceTuple v =
  bitraverse (v ^?) (\p -> v ^? p <&> toRational)
    ( key "t" . _TickerSymbol, key "l" . _String . _Number )


-- Optics

_TickerSymbol :: Prism' AT.Value TickerSymbol
_TickerSymbol =
  _String . _Text . _Show

_Symbol :: Lens' Holding TickerSymbol
_Symbol =
  lens (\(Holding s _ _) -> s) (\(Holding _ h t) s -> Holding s h t)

_Shares :: Lens' Holding Int
_Shares =
  lens (\(Holding _ h _) -> h) (\(Holding s _ t) h -> Holding s h t)

_Target :: Lens' Holding Rational
_Target =
  lens (\(Holding _ _ t) -> t) (\(Holding s h _) t -> Holding s h t)

googleFinanceAPI :: String
googleFinanceAPI =
  "https://finance.google.com/finance/info?client=ig&alt=json&q="
    <> (List.intercalate "," $ show <$> [ (minBound :: TickerSymbol) .. ])

holdingsTotal :: Map TickerSymbol Rational -> Rational -> [Holding] -> Rational
holdingsTotal prices =
  foldl $ \total (Holding s h _) ->
    total + ((Map.findWithDefault 0.0 s prices) * toRational h)

-- Mean of the deltas between a pair of holdings
holdingsTargetDelta :: [Holding] -> [Holding] -> Rational
holdingsTargetDelta xs ys =
  abs . mean $ zipWith (\(Holding _ _ tx) (Holding _ _ ty) -> tx - ty) xs ys
  where
    mean :: [Rational] -> Rational
    mean = \case
      [] -> 1000.0  -- really far-off number
      zs -> sum zs / (fromIntegral . length) zs

-- Increments holding, then recalculates the percentages
incHoldingWhere :: Map TickerSymbol Rational -> Rational -> TickerSymbol -> E [Holding]
incHoldingWhere prices value sym =
  map (\h -> if h ^. _Symbol == sym then h & _Shares +~ 1 else h)
    >>> map (\(Holding s h _) -> Holding s h (Map.findWithDefault 0.0 s prices * (toRational h) / value))

pricesToTest :: Rational -> E (Map TickerSymbol Rational)
pricesToTest leftoverCash =
  Map.filter (leftoverCash >)

-- TODO: this is garbage
refine :: Map TickerSymbol Rational -> Rational -> (Rational, [Holding])
refine prices totalValue =
  ref (pricesToTest leftC prices)
    leftC
    ( (holdingsTargetDelta holdings estimate)
    , estimate
    )
  where
    leftC :: Rational
    leftC =
      totalValue - holdingsTotal prices 0.0 holdings

    underEstimate :: E Holding
    underEstimate (Holding s _ t) =
      let
        price = Map.findWithDefault 0.0 s prices
        shares = floor $ t * totalValue / price
      in
        Holding s shares ((fromInteger shares) * price / totalValue)

    -- This becomes the base as we know we'll have extra values
    estimate :: [Holding]
    estimate =
      underEstimate <$> holdings

    -- This would be better solved via a 'coin counting' algorithm
    ref :: Map TickerSymbol Rational -> Rational -> E (Rational, [Holding])
    ref filtPrices leftoverCash initial =
      if Map.null filtPrices then
        initial
      else
        let
          incH :: TickerSymbol -> E [Holding]
          incH =
            incHoldingWhere prices totalValue

          testValues :: (Rational, [Holding]) -> TickerSymbol -> Rational -> (Rational, [Holding])
          testValues acc@( d, hs ) sym _price =
            let
              hs' :: [Holding]
              hs' =
                incH sym hs

              d' :: Rational
              d' =
                holdingsTargetDelta hs hs'
            in
              if d' < d then
                ( d', hs' )
              else
                acc

          best :: (Rational, [Holding])
          best =
            Map.foldlWithKey' testValues initial filtPrices
        in
          if best == initial then
            ( leftoverCash, initial ^. _2 )
          else
            let
              lc :: Rational
              lc =
                totalValue  - (holdingsTotal prices 0.0 $ best ^. _2)
            in
              ref (pricesToTest lc filtPrices) lc best

main :: IO ()
main = do
  let
    wreqOpts :: Wreq.Options
    wreqOpts =
      Wreq.defaults & Wreq.header "Accept" .~  [ "application/json" ]
  r :: Wreq.Response BS.ByteString <-
    Wreq.getWith wreqOpts googleFinanceAPI
      <&> over Wreq.responseBody (BS.dropWhile ((/=) $ BSI.c2w '['))
  let
    -- Put in State
    prices :: Map TickerSymbol Rational
    prices =
      Map.fromList . Maybe.mapMaybe toStockPriceTuple $ r ^.. Wreq.responseBody . values

    -- TODO insert CLI var
    totalValue :: Rational
    totalValue =
      holdingsTotal prices (1000.0) holdings

    ( leftoverCash, newHoldings ) =
      refine prices totalValue

  putStrLn $ "Total: $" <> show (fromRational totalValue)
  mapM_ print newHoldings
  putStrLn $ "Cash: $" <> show (fromIntegral (round $ leftoverCash * 100) / 100)
