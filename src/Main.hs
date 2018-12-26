{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}

module Main where

import           Prelude

import           Control.Arrow            ((>>>))
import           Control.Lens
import           Control.Lens.TH          (makeLenses)
import           Control.Lens.Tuple
import           Data.Aeson.Lens          (AsValue, key, members, values,
                                           _Number, _Object, _String)
import           Data.Aeson.Types         as AT
import           Data.Bitraversable       (bitraverse)
import qualified Data.ByteString.Internal as BSI
import qualified Data.ByteString.Lazy     as BS
import qualified Data.HashMap.Lazy        (HashMap)
import qualified Data.HashMap.Lazy        as HashMap
import qualified Data.List                as List
import           Data.Map.Lazy            (Map)
import qualified Data.Map.Lazy            as Map
import qualified Data.Maybe               as Maybe
import           Data.Monoid              ((<>))
import           Data.Monoid.Endo         (E)
import           Data.Text                (Text)
import           Data.Text.Lens           (_Text)
import           Debug.Trace
import qualified Network.Wreq             as Wreq

newtype Price = Price { unprice ∷ Rational }
  deriving (Fractional, Eq, Num, Ord, Real, RealFrac, Show)

newtype Share = Share { unshare ∷ Int }
  deriving (Eq, Num, Ord, Real, Show)

newtype Target = Target { untarget ∷ Rational }
  deriving (Fractional, Eq, Num, Ord, Real, RealFrac, Show)

-- Edit These

-- Available Symbols
data Symbol
  = SCHB
  | SCHF
  | SCHE
  | SCHD
  | VTEB
  | BWX
  | SCHP
  | GII
  | FUTY
  | PICK
  | GLTR
  | CGW
  deriving (Bounded, Enum, Eq, Ord, Read, Show)

-- Schwab Zero-Commission ETFs @ Lower Risk
-- Currently I have 6 years of free trading so I'm choosing VTEB
-- over TFI & PICK over PSAU, saving 0.14% & 0.36% in expennse
-- ratios respectively.
-- http://www.schwab.com/public/schwab/investing/investment_help/investment_research/etf_research/etfs.html?&path=/Prospect/Research/etfs/overview/oneSourceETFs.aspw
positions ∷ [Position]
positions =
  [ Position SCHB 32 0.23  -- 0.03 US Broad
  , Position SCHF 40 0.15  -- 0.06 Foreign Developed
  , Position SCHE 44 0.14  -- 0.13 Emerging Markets
  , Position SCHD 16 0.10  -- 0.07 US Dividend
  , Position VTEB 35 0.20  -- 0.09 US Municipal Bond (0.23 TFI)
  , Position SCHP  6 0.04  -- 0.05 US TIPS
  , Position BWX  15 0.05  -- 0.50 Intl. Treasury Bond
  , Position GII   5 0.03  -- 0.40 Global Infrastructure
  , Position FUTY  5 0.02  -- 0.08 US Utilities
  , Position CGW   5 0.02  -- 0.64 Water
  , Position PICK  2 0.01  -- 0.39 Mining (0.75 PSAU)
  , Position GLTR  1 0.01  -- 0.60 Precious Metals
  ]

-- Symbol, Current Shares, Percent Target
data Position = Position
  { _symbol ∷ Symbol
  , _shares ∷ Share
  , _target ∷ Target
  } deriving (Eq)

makeLenses ''Position

-- TODO: yeah
instance Show Position where
  show (Position s h t) =
    show s <> " : " <> show (unshare h) <> " - " <> show (fromIntegral (round $ t * 1000) / 10) <> "%"

type StockPrices =
  Map Symbol Price

_Symbol ∷ Prism' AT.Value Symbol
_Symbol =
  _String . _Text . _Show

data Possibility a
  = Node a [Possibility a]
  deriving (Eq, Read, Show)

--respObjToStockPrices ∷ AsValue t ⇒ ( Text, t ) → StockPrices
respObjToStockPrices ∷ HashMap.HashMap Text Value → StockPrices
respObjToStockPrices =
  HashMap.foldrWithKey fn Map.empty
  where
    fn ∷ Text → Value → E StockPrices
    fn sym value acc =
      case
        ( sym ^? _Text . _Show
        , value ^? key "quote" . key "latestPrice" . _Number <&> Price . toRational
        )
      of
        ( Just symbol, Just value ) → Map.insert symbol value acc
        _                           → acc


--https://iextrading.com/developer/docs/#attribution
iexTradingAPI ∷ String
iexTradingAPI =
  "https://api.iextrading.com/1.0/stock/market/batch?types=quote&filter=symbol,latestPrice&symbols="
    <> (List.intercalate "," $ show <$> [ (minBound ∷ Symbol) .. ])

positionsTotal ∷ StockPrices → Rational → [Position] → Rational
positionsTotal prices =
  foldl $ \total h →
    let
      price ∷ Rational
      price = maybe 0.0 unprice $ Map.lookup (h ^. symbol) prices
    in
      total + (price * toRational (h ^. shares))


-- Mean of the deltas between a pair of positions
positionsTargetDelta ∷ [Position] → [Position] → Rational
positionsTargetDelta xs ys =
  abs . mean $ zipWith (\x y → untarget $ diff x y) xs ys
  where
    diff ∷ Position → Position → Target
    diff x y =
      (x ^. target) - (y ^. target)

    mean ∷ [Rational] → Rational
    mean = \case
      [] → 100000.0  -- really far-off number
      zs → sum zs / (fromIntegral . length) zs

-- Increments position, then recalculates the percentages
incPositionWhere ∷ StockPrices → Rational → Symbol → E [Position]
incPositionWhere prices value sym =
  map (\pos →
    let
      h ∷ Position
      h = if pos ^. symbol == sym then pos & shares +~ 1 else pos

      p ∷ Rational
      p = maybe 0.0 unprice $ Map.lookup (h ^. symbol) prices
    in
      h & target .~ Target (p * (h ^. shares & toRational) / value)
  )


--redistribute ∷ StockPrices → Rational → (Rational, [Position])
{-
redistribute totalValue prices =
  possible leftoverAfterEstimate (pricesToTest leftoverAfterEstimate $ Map.toList prices)
  where
    underEstimate ∷ E Position
    underEstimate (Position s _ t) =
      let
        price ∷ Rational
        price = maybe 0.0 unprice $ Map.lookup s prices

        share' ∷ Int
        share' = floor $ (untarget t) * totalValue / price
      in
        Position s (Share share') (Target $ (fromIntegral share') * price / totalValue)

    -- This becomes the base as we know we'll have extra values
    estimate ∷ [Position]
    estimate =
      underEstimate <$> positions

    leftoverAfterEstimate ∷ Rational
    leftoverAfterEstimate =
      totalValue - positionsTotal prices 0.0 estimate

    pricesToTest ∷ Rational → E [(Symbol, Price)]
    pricesToTest leftover =
      --traceShowId .
      filter (\(_, p) → leftover > unprice p)

    possible ∷ Rational → [(Symbol, Price)] → [[Symbol]]
    possible lo ps = [] : possible' lo ps

    -- this foldr just doesn't seem to be doing it... I'm losing the leftover value
    possible' ∷ Rational → [(Symbol, Price)] → [[Symbol]]
    possible' _ [] = []
    possible' lo (x@(t, p):ps) =
      let
        leftover ∷ Rational
        leftover = traceShow (fromRational $ lo - unprice p) (lo - unprice p)

        nextPrices ∷ [(Symbol, Price)]
        nextPrices = pricesToTest leftover (ps <> [traceShow (x ^. _2 & unprice & fromRational) x])

        foldover ∷ [Symbol] → [[Symbol]] → [[Symbol]]
        foldover ts symbols =
          ts : (t : ts) : symbols
      in
        -- guard
        if leftover < 0 then [] else [t] : foldr foldover [] (possible' leftover nextPrices)
-}

-- TODO: this is garbage
{-
refine ∷ StockPrices → Rational → (Rational, [Position])
refine prices totalValue =
  ref (pricesToTest leftC prices)
    leftC
    ( (positionsTargetDelta positions estimate)
    , estimate
    )
  where
    pricesToTest ∷ Rational → E (StockPrices)
    pricesToTest leftoverCash =
      Map.filter (Price leftoverCash >)

    leftC ∷ Rational
    leftC =
      totalValue - positionsTotal prices 0.0 positions

    underEstimate ∷ E Position
    underEstimate (Position s _ t) =
      let
        price ∷ Rational
        price = maybe 0.0 unprice $ Map.lookup s prices

        share' ∷ Int
        share' = floor $ (untarget t) * totalValue / price
      in
        Position s (Share share') (Target $ (fromIntegral share') * price / totalValue)

    -- This becomes the base as we know we'll have extra values
    estimate ∷ [Position]
    estimate =
      underEstimate <$> positions

    -- This would be better solved via a 'coin counting' algorithm
    ref ∷ StockPrices → Rational → E (Rational, [Position])
    ref filtPrices leftoverCash initial =
      if Map.null filtPrices then
        initial
      else
        let
          incH ∷ Symbol → E [Position]
          incH =
            incPositionWhere prices totalValue

          testValues ∷ (Rational, [Position]) → Symbol → Price → (Rational, [Position])
          testValues acc@( d, hs ) sym _price =
            let
              hs' ∷ [Position]
              hs' =
                incH sym hs

              d' ∷ Rational
              d' =
                positionsTargetDelta hs hs'
            in
              if d' < d then
                ( d', hs' )
              else
                acc

          best ∷ (Rational, [Position])
          best =
            Map.foldlWithKey' testValues initial filtPrices
        in
          if best == initial then
            ( leftoverCash, initial ^. _2 )
          else
            let
              lc ∷ Rational
              lc =
                totalValue  - (positionsTotal prices 0.0 $ best ^. _2)
            in
              ref (pricesToTest lc filtPrices) lc best
-}

coarseDistribute ∷ StockPrices → Rational → [Position]
coarseDistribute prices totalValue =
  underEstimate <$> positions
  where
    underEstimate ∷ E Position
    underEstimate (Position s _ t) =
      Position s (Share share') (Target $ (fromIntegral share') * price / totalValue)
      where
        price ∷ Rational
        price = maybe 0.0 unprice $ Map.lookup s prices

        share' ∷ Int
        share' = floor $ (untarget t) * totalValue / price

main ∷ IO ()
main = do
  putStr "Amount to transfer: $"
  transfer ∷ Float ← readLn
  putStrLn "-------------------hashmap adt haskell-------------------"
  let wreqOpts ∷ Wreq.Options = Wreq.defaults & Wreq.header "Accept" .~  [ "application/json" ]
  r ∷ Wreq.Response BS.ByteString ← Wreq.getWith wreqOpts iexTradingAPI
  let
    prices ∷ StockPrices
    prices =
      r ^.. Wreq.responseBody . _Object ^? element 0 & maybe Map.empty respObjToStockPrices

    -- TODO insert CLI var
    totalValue ∷ Rational
    totalValue =
      positionsTotal prices (toRational transfer) positions

    newPositions ∷ [Position]
    newPositions =
      coarseDistribute prices totalValue

    leftoverCash ∷ Rational
    leftoverCash =
      positionsTotal prices 0 newPositions
  print prices
  mapM_ print newPositions
  putStrLn $ "Cash: $" <> show (fromIntegral (round $ leftoverCash * 100) / 100)
  putStrLn $ "Total: $" <> show (fromRational totalValue)
  --mapM_ print $  redistribute totalValue prices
  putStrLn "fin."
