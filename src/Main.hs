{-# LANGUAGE LambdaCase, NoMonomorphismRestriction, OverloadedStrings, ScopedTypeVariables, TemplateHaskell #-}

module Main where

import Prelude

import Control.Arrow ((>>>))
import Control.Lens
import Control.Lens.TH (makeLenses)
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
data Ticker
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

-- Schwab Zero-Commission ETFs @ Lower Risk
-- http://www.schwab.com/public/schwab/investing/investment_help/investment_research/etf_research/etfs.html?&path=/Prospect/Research/etfs/overview/oneSourceETFs.asp
holdings :: [Holding]
holdings =
  [ Holding SCHB 18 0.27  -- US Broad
  , Holding SCHF 18 0.15  -- Foreign Developed
  , Holding SCHE 16 0.11  -- Emerging Markets
  , Holding SCHD  7 0.09  -- US Dividend
  , Holding TFI  14 0.20  -- Municipal Bond
  , Holding BWX   8 0.06  -- Intl. Treasury Bond
  , Holding SCHP  2 0.04  -- TIPS
  , Holding GII   3 0.04  -- Global Infrastructure
  , Holding PSAU  5 0.01  -- Mining
  , Holding GLTR  0 0.01  -- Precious Metals
  , Holding CGW   1 0.02  -- Water
  ]

-- Symbol, Current Shares, Percent Target
data Holding = Holding
  { _ticker :: Ticker
  , _shares :: Int
  , _target :: Rational
  } deriving (Eq)

makeLenses ''Holding

-- TODO: yeah
instance Show Holding where
  show (Holding s h t) =
    show s <> " : " <> show h <> " - " <> show (fromIntegral (round $ t * 1000) / 10) <> "%"

_Ticker :: Prism' AT.Value Ticker
_Ticker =
  _String . _Text . _Show

toStockPriceTuple :: AsValue t => t -> Maybe ( Ticker, Rational )
toStockPriceTuple v =
  bitraverse (v ^?) (\p -> v ^? p <&> toRational)
    ( key "t" . _Ticker, key "l" . _String . _Number )

googleFinanceAPI :: String
googleFinanceAPI =
  "https://finance.google.com/finance/info?client=ig&alt=json&q="
    <> (List.intercalate "," $ show <$> [ (minBound :: Ticker) .. ])

holdingsTotal :: Map Ticker Rational -> Rational -> [Holding] -> Rational
holdingsTotal prices =
  foldl $ \total h ->
    let price = Map.findWithDefault 0.0 (h ^. ticker) prices
    in total + (price * toRational (h ^. shares))


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
incHoldingWhere :: Map Ticker Rational -> Rational -> Ticker -> E [Holding]
incHoldingWhere prices value sym =
  map (\h -> if h ^. ticker == sym then h & shares +~ 1 else h)
    >>> map (\(Holding s h _) -> Holding s h (Map.findWithDefault 0.0 s prices * (toRational h) / value))

pricesToTest :: Rational -> E (Map Ticker Rational)
pricesToTest leftoverCash =
  Map.filter (leftoverCash >)

-- TODO: this is garbage
refine :: Map Ticker Rational -> Rational -> (Rational, [Holding])
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
        shares' = floor $ t * totalValue / price
      in
        Holding s shares' ((fromInteger shares') * price / totalValue)

    -- This becomes the base as we know we'll have extra values
    estimate :: [Holding]
    estimate =
      underEstimate <$> holdings

    -- This would be better solved via a 'coin counting' algorithm
    ref :: Map Ticker Rational -> Rational -> E (Rational, [Holding])
    ref filtPrices leftoverCash initial =
      if Map.null filtPrices then
        initial
      else
        let
          incH :: Ticker -> E [Holding]
          incH =
            incHoldingWhere prices totalValue

          testValues :: (Rational, [Holding]) -> Ticker -> Rational -> (Rational, [Holding])
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
  putStr "Amount to transfer: $"
  transfer :: Float <- readLn
  putStrLn "--------------------------------------"
  let
    wreqOpts :: Wreq.Options
    wreqOpts =
      Wreq.defaults & Wreq.header "Accept" .~  [ "application/json" ]
  r :: Wreq.Response BS.ByteString <-
    Wreq.getWith wreqOpts googleFinanceAPI
      <&> over Wreq.responseBody (BS.dropWhile ((/=) $ BSI.c2w '['))
  let
    -- Put in State
    prices :: Map Ticker Rational
    prices =
      Map.fromList . Maybe.mapMaybe toStockPriceTuple $ r ^.. Wreq.responseBody . values

    -- TODO insert CLI var
    totalValue :: Rational
    totalValue =
      holdingsTotal prices (toRational transfer) holdings

    ( leftoverCash, newHoldings ) =
      refine prices totalValue

  mapM_ print newHoldings
  putStrLn $ "Cash: $" <> show (fromIntegral (round $ leftoverCash * 100) / 100)
  putStrLn $ "Total: $" <> show (fromRational totalValue)
