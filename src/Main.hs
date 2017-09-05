{-# LANGUAGE GeneralizedNewtypeDeriving, LambdaCase, OverloadedStrings, ScopedTypeVariables, TemplateHaskell #-}

module Main where

import Debug.Trace
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

newtype Price = Price { unprice :: Rational }
  deriving (Fractional, Eq, Num, Ord, Real, RealFrac, Show)

newtype Share = Share { unshare :: Int }
  deriving (Eq, Num, Ord, Real, Show)

newtype Target = Target { untarget :: Rational }
  deriving (Fractional, Eq, Num, Ord, Real, RealFrac, Show)

-- Edit These

-- Available Ticker Symbols
data Ticker
  = SCHB
  | SCHF
  | SCHE
  | SCHD
  | VTEB
  | BWX
  | SCHP
  | GII
  | FUTY
  | PSAU
  | PICK
  | GLTR
  | CGW
  deriving (Bounded, Enum, Eq, Ord, Read, Show)

-- Schwab Zero-Commission ETFs @ Lower Risk
-- Currently I have 6 years of free trading so I'm choosing VTEB
-- over TFI & PICK over PSAU, saving 0.14% & 0.36% respectively.
-- http://www.schwab.com/public/schwab/investing/investment_help/investment_research/etf_research/etfs.html?&path=/Prospect/Research/etfs/overview/oneSourceETFs.aspw
positions :: [Position]
positions =
  [ Position SCHB 35 0.22  -- 0.03 US Broad
  , Position SCHF 44 0.15  -- 0.06 Foreign Developed
  , Position SCHE 52 0.15  -- 0.13 Emerging Markets
  , Position SCHD 16 0.08  -- 0.07 US Dividend
  , Position VTEB 38 0.21  -- 0.09 US Municipal Bond (0.23 TFI)
  , Position SCHP  6 0.04  -- 0.05 US TIPS
  , Position BWX  19 0.06  -- 0.50 Intl. Treasury Bond
  , Position GII   5 0.03  -- 0.40 Global Infrastructure
  , Position FUTY  5 0.02  -- 0.08 US Utilities
  , Position PSAU  5 0.00  -- 0.75 Mining
  , Position PICK  3 0.01  -- 0.39 Mining (0.75 PSAU)
  , Position GLTR  1 0.01  -- 0.60 Precious Metals
  , Position CGW   5 0.02  -- 0.64 Water
  ]

-- Symbol, Current Shares, Percent Target
data Position = Position
  { _ticker :: Ticker
  , _shares :: Share
  , _target :: Target
  } deriving (Eq)

makeLenses ''Position

-- TODO: yeah
instance Show Position where
  show (Position s h t) =
    show s <> " : " <> show (unshare h) <> " - " <> show (fromIntegral (round $ t * 1000) / 10) <> "%"

type StockPrices = Map Ticker Price

_Ticker :: Prism' AT.Value Ticker
_Ticker =
  _String . _Text . _Show

toStockPriceTuple :: AsValue t => t -> Maybe ( Ticker, Price )
toStockPriceTuple v =
  bitraverse (v ^?) (\p -> v ^? p <&> Price . toRational)
    ( key "t" . _Ticker, key "l" . _String . _Number )

googleFinanceAPI :: String
googleFinanceAPI =
  "https://finance.google.com/finance/info?client=ig&alt=json&q="
    <> (List.intercalate "," $ show <$> [ (minBound :: Ticker) .. ])

positionsTotal :: StockPrices -> Rational -> [Position] -> Rational
positionsTotal prices =
  foldl $ \total h ->
    let price = Map.findWithDefault 0.0 (h ^. ticker) prices
    in total + (unprice price * toRational (h ^. shares))


-- Mean of the deltas between a pair of positions
positionsTargetDelta :: [Position] -> [Position] -> Rational
positionsTargetDelta xs ys =
  abs . mean $ zipWith ((fmap . fmap) untarget subtract') xs ys
  where
    subtract' :: Position -> Position -> Target
    subtract' x y =
      (x ^. target) - (y ^. target)

    mean :: [Rational] -> Rational
    mean = \case
      [] -> 1000.0  -- really far-off number
      zs -> sum zs / (fromIntegral . length) zs

-- Increments position, then recalculates the percentages
incPositionWhere :: StockPrices -> Rational -> Ticker -> E [Position]
incPositionWhere prices value sym =
  map (\pos ->
    let
      h :: Position
      h = if pos ^. ticker == sym then pos & shares +~ 1 else pos

      p :: Rational
      p = unprice $ Map.findWithDefault 0.0 (h ^. ticker) prices
    in
      h & target .~ Target (p * (h ^. shares & toRational) / value)
  )


--redistribute :: StockPrices -> Rational -> (Rational, [Position])
redistribute totalValue prices =
  possible leftoverAfterEstimate (pricesToTest leftoverAfterEstimate $ Map.toList prices)
  where
    underEstimate :: E Position
    underEstimate (Position s _ t) =
      let
        price :: Rational
        price =
          unprice $ Map.findWithDefault 0.0 s prices

        share' :: Int
        share' =
          floor $ (untarget t) * totalValue / price
      in
        Position s (Share share') (Target $ (fromIntegral share') * price / totalValue)

    -- This becomes the base as we know we'll have extra values
    estimate :: [Position]
    estimate =
      underEstimate <$> positions

    leftoverAfterEstimate :: Rational
    leftoverAfterEstimate =
      totalValue - positionsTotal prices 0.0 estimate

    pricesToTest :: Rational -> E [(Ticker, Price)]
    pricesToTest leftover =
      --traceShowId .
      filter (\(_, p) -> leftover > unprice p)

    possible :: Rational -> [(Ticker, Price)] -> [[Ticker]]
    possible lo ps = [] : possible' lo ps

    -- this foldr just doesn't seem to be doing it... I'm losing the leftover value
    possible' :: Rational -> [(Ticker, Price)] -> [[Ticker]]
    possible' _ [] = []
    possible' lo (x@(t, p):ps) =
      let
        leftover :: Rational
        leftover = traceShow (fromRational $ lo - unprice p) (lo - unprice p)

        nextPrices :: [(Ticker, Price)]
        nextPrices = pricesToTest leftover (ps <> [traceShow (x ^. _2 & unprice & fromRational) x])

        foldover :: [Ticker] -> [[Ticker]] -> [[Ticker]]
        foldover ts tickers =
          ts : (t : ts) : tickers
      in
        -- guard
        if leftover < 0 then [] else [t] : foldr foldover [] (possible' leftover nextPrices)

-- TODO: this is garbage
refine :: StockPrices -> Rational -> (Rational, [Position])
refine prices totalValue =
  ref (pricesToTest leftC prices)
    leftC
    ( (positionsTargetDelta positions estimate)
    , estimate
    )
  where
    pricesToTest :: Rational -> E (StockPrices)
    pricesToTest leftoverCash =
      Map.filter (Price leftoverCash >)

    leftC :: Rational
    leftC =
      totalValue - positionsTotal prices 0.0 positions

    underEstimate :: E Position
    underEstimate (Position s _ t) =
      let
        price :: Rational
        price =
          unprice $ Map.findWithDefault 0.0 s prices

        share' :: Int
        share' =
          floor $ (untarget t) * totalValue / price
      in
        Position s (Share share') (Target $ (fromIntegral share') * price / totalValue)

    -- This becomes the base as we know we'll have extra values
    estimate :: [Position]
    estimate =
      underEstimate <$> positions

    -- This would be better solved via a 'coin counting' algorithm
    ref :: StockPrices -> Rational -> E (Rational, [Position])
    ref filtPrices leftoverCash initial =
      if Map.null filtPrices then
        initial
      else
        let
          incH :: Ticker -> E [Position]
          incH =
            incPositionWhere prices totalValue

          testValues :: (Rational, [Position]) -> Ticker -> Price -> (Rational, [Position])
          testValues acc@( d, hs ) sym _price =
            let
              hs' :: [Position]
              hs' =
                incH sym hs

              d' :: Rational
              d' =
                positionsTargetDelta hs hs'
            in
              if d' < d then
                ( d', hs' )
              else
                acc

          best :: (Rational, [Position])
          best =
            Map.foldlWithKey' testValues initial filtPrices
        in
          if best == initial then
            ( leftoverCash, initial ^. _2 )
          else
            let
              lc :: Rational
              lc =
                totalValue  - (positionsTotal prices 0.0 $ best ^. _2)
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
    prices :: StockPrices
    prices =
      Map.fromList . Maybe.mapMaybe toStockPriceTuple $ r ^.. Wreq.responseBody . values

    -- TODO insert CLI var
    totalValue :: Rational
    totalValue =
      positionsTotal prices (toRational transfer) positions

    ( leftoverCash, newPositions ) =
      refine prices totalValue


  mapM_ print newPositions
  putStrLn $ "Cash: $" <> show (fromIntegral (round $ leftoverCash * 100) / 100)
  putStrLn $ "Total: $" <> show (fromRational totalValue)
  --mapM_ print $  redistribute totalValue prices
