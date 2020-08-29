{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
module Main
( main )
where


import           Prelude                                    hiding (log)
import qualified Options                                    as Opt
import qualified Format
import           OrderBook.Graph.Internal.Prelude           hiding (log)
import qualified OrderBook.Graph.Internal.Util              as Util
import           OrderBook.Graph.Types                      (SomeSellOrder, SomeSellOrder'(..))
import           OrderBook.Graph.Types.Book                 (OrderBook)
import qualified OrderBook.Graph.Types.Book                 as Book
import qualified OrderBook.Graph                            as Lib

import qualified Control.Monad.ST                           as ST
import qualified Data.Graph.Digraph                         as DG
import           Data.List                                  (sortBy, nub, (\\), sortOn)
import           Data.Ord                                   (comparing)

import qualified Data.List.NonEmpty                         as NE
import qualified Data.Text                                  as T
import qualified Data.Aeson                                 as Json
import           Data.Aeson                                 ((.=))
import           System.FilePath                            ((</>))
import qualified System.FilePath                            as FP
import qualified Criterion
import qualified Criterion.Main                             as Criterion
import qualified Criterion.Main.Options                     as Criterion
import qualified Criterion.Types                            as Criterion
import qualified Control.Logging                            as Log
import           System.IO.Unsafe                           (unsafePerformIO)
import qualified UnliftIO.Async                             as Async
import qualified Data.Csv.Incremental                       as Csv


main :: IO ()
main = Opt.withOptions $ \options ->
    Opt.withNumberType options $ \(Opt.SomeNumberType (_ :: Proxy numType)) ->
    forM_ (Opt.inputFiles options) $ \inputFile -> do
        orderBooks :: [OrderBook numType] <- readOrdersFile options inputFile
        (graphInfo, _)  <- ST.stToIO $ buildGraph orderBooks
        let executionCryptoList = mkExecutions options graphInfo inputFile orderBooks
        logResult <- forAll (Opt.mode options) executionCryptoList $ \(execution, crypto) -> do
            case Opt.mode options of
                    Opt.Analyze ->
                        (Just . showExecutionResult) <$> analyze crypto options execution
                    Opt.AnalyzeCsv ->
                        (Just . csvExecutionResult) <$> analyze crypto options execution
                    Opt.Visualize outputDir -> do
                        visualize options crypto outputDir execution
                        return Nothing
                    Opt.Benchmark -> do
                        benchmark Nothing execution
                        return Nothing
                    Opt.BenchmarkCsv csvOut -> do
                        benchmark (Just csvOut) execution
                        return Nothing
        forM_ (catMaybes logResult) putStr
  where
    csvExecutionResult er = toS . Csv.encode $
        let liquidity sideM = fromMaybe 0 $ liLiquidity <$> (liLiquidityInfo er >>= sideM)
        in csvOutput er (liquidity liBuyLiquidity) (liquidity liSellLiquidity)
    csvOutput
        :: ExecutionResult
        -> Lib.NumType
        -> Lib.NumType
           -- (file path, base/quote, max slippage, buy liquidity, sell liquidity, sum liquidity)
        -> Csv.Builder (FilePath, String, Double, Integer, Integer, Integer)
    csvOutput ExecutionResult{..} buyLiquidity sellLiquidity =
        Csv.encodeRecord
            ( liInputFile
            , show liCrypto ++ "/" ++ show liNumeraire
            , liMaxSlippage
            , round buyLiquidity                   :: Integer
            , round sellLiquidity                  :: Integer
            , round $ buyLiquidity + sellLiquidity :: Integer
            )

-- Parallelize everything, unless it's related to measuring speed/performance
forAll :: Opt.Mode
          -- ^ Mode
       -> [a]
          -- ^ Input list
       -> (a -> IO (Maybe String))
          -- ^ Do for all list items; return output
       -> IO [Maybe String]
          -- ^ All outputs
forAll  Opt.AnalyzeCsv   =
            let addCsvHeader = fmap (fmap (Just csvHeader :))
            in addCsvHeader . concurrent
forAll  Opt.Analyze         = concurrent
forAll (Opt.Visualize _)    = concurrent
forAll  Opt.Benchmark       = sequential
forAll (Opt.BenchmarkCsv _) = sequential

concurrent :: [a] -> (a -> IO b) -> IO [b]
concurrent = flip Async.pooledMapConcurrently

sequential :: [a] -> (a -> IO b) -> IO [b]
sequential = flip mapM

csvHeader :: String
csvHeader = toS . Csv.encode $ Csv.encodeRecord
    ( "file"
    , "market"
    , "max_slippage"
    , "buy_liquidity"
    , "sell_liquidity"
    , "sum_liquidity"
    )

data Execution numType = Execution
    { inputFile     :: FilePath
      -- ^ Input order book file
    , graphInfo     :: GraphInfo numType
      -- ^ Information about the graph
    , inputData     :: [OrderBook numType]
      -- ^ Order books read from 'inputFile'
    , mainRun       :: [OrderBook numType] -> IO ([SomeSellOrder], [SomeSellOrder])
      -- ^ Process input data
    }

mkExecutions
    :: (Json.FromJSON numType, Fractional numType, Real numType)
    => Opt.Options
    -> GraphInfo numType
    -> FilePath
    -> [OrderBook numType]
    -> [(Execution numType, Lib.Currency)]
mkExecutions options graphInfo inputFile orderBooks = do
    map (\crypto -> (mkExecution crypto, crypto)) allCryptos
  where
    allCryptos = case Opt.crypto options of
            Opt.OneOrMore cryptos -> NE.toList cryptos
            Opt.AllCryptos    -> giVertices graphInfo \\ [numeraire]
    mkExecution crypto =
        Execution inputFile graphInfo orderBooks (mainRun crypto)
    mainRun crypto orders =
        withBidsAsksOrder numeraire crypto $ \buyOrder sellOrder ->
            matchOrders options buyOrder sellOrder orders
    numeraire   = Opt.numeraire options

data PriceRange numType =
    PriceRange
        { lowestPrice :: numType
        , highestPrice :: numType
        }

-- | Result for an entire execution
data ExecutionResult = ExecutionResult
    { liInputFile       :: FilePath
    , liMaxSlippage     :: Double
    , liCrypto          :: Lib.Currency     -- ^ Target cryptocurrency
    , liNumeraire       :: Lib.Currency     -- ^ Numeraire
    , liLiquidityInfo   :: Maybe LiquidityInfo
    }

-- | Liquidity info in both buy and sell direction
data LiquidityInfo = LiquidityInfo
    { liBaseQuote       :: (Lib.Currency, Lib.Currency)
    , liBuyLiquidity    :: Maybe SideLiquidity
    , liSellLiquidity   :: Maybe SideLiquidity
    }

-- | Liquidity info in a single direction (either buy or sell)
data SideLiquidity = SideLiquidity
    { liLiquidity    :: Lib.NumType             -- ^ Non-zero liquidity
    , liPriceRange   :: PriceRange Lib.NumType
    , liPaths        :: NonEmpty (Lib.NumType, PriceRange Lib.NumType, T.Text)  -- ^ (quantity, price_range, path_description)
    }

analyze :: Lib.Currency -> Opt.Options -> Execution numType -> IO ExecutionResult
analyze cryptocurrency Opt.Options{..} Execution{..} = do
    (buyOrders, sellOrders) <- mainRun inputData
    return $ ExecutionResult
        { liInputFile       = inputFile
        , liMaxSlippage     = maxSlippage
        , liCrypto          = cryptocurrency
        , liNumeraire       = numeraire
        , liLiquidityInfo   = toLiquidityInfo maxNumPaths (buyOrders, sellOrders)
        }

toLiquidityInfo
    :: Int
    -> ([SomeSellOrder' Lib.NumType], [SomeSellOrder' Lib.NumType])
    -> Maybe LiquidityInfo
toLiquidityInfo _ ([], []) = Nothing
toLiquidityInfo maxNumPaths (buyOrders, sellOrders) = Just $
    LiquidityInfo
        { liBaseQuote       = ordersMarket allOrders
        , liBuyLiquidity    = toSideLiquidity maxNumPaths sellOrders
        , liSellLiquidity   = toSideLiquidity maxNumPaths buyOrders
        }
  where
    allOrders = NE.fromList $ buyOrders ++ sellOrders
    ordersMarket nonEmptyOrders = orderMarket (NE.head nonEmptyOrders)
    orderMarket order = (Lib.soBase order, Lib.soQuote order)

toSideLiquidity
    :: Int
    -> [SomeSellOrder' Lib.NumType]
    -> Maybe SideLiquidity
toSideLiquidity _ [] = Nothing
toSideLiquidity maxNumPaths _
    | maxNumPaths <= 0 = Nothing
toSideLiquidity maxNumPaths nonEmptyOrders = Just $
    let paths = take maxNumPaths $ sortByQuantity $ map quoteSumVenue (groupByVenue nonEmptyOrders)
    in SideLiquidity
        { liLiquidity    = quoteSum nonEmptyOrders
        , liPriceRange   = firstLastPrice $ NE.fromList nonEmptyOrders
        , liPaths        = NE.fromList paths    -- Will not fail since "maxNumPaths > 0"
        }
  where
    firstLastPrice lst =
        let priceSorted = NE.sortBy (comparing soPrice) lst
        in PriceRange (soPrice $ NE.head priceSorted) (soPrice $ NE.last priceSorted)
    quoteSumVenue orders =
        (quoteSum $ NE.toList orders, priceRange orders, soVenue $ NE.head orders)
    groupByVenue = NE.groupBy (\a b -> soVenue a == soVenue b) . sortOn soVenue
    sortByQuantity = sortBy (flip $ comparing $ \(quoteSum, _, _) -> quoteSum)
    quoteSum orderList = sum $ map quoteQuantity orderList
    quoteQuantity order = Lib.soQty order * Lib.soPrice order
    priceRange
        :: NE.NonEmpty SomeSellOrder
        -> PriceRange Lib.NumType
    priceRange soList =
        let priceList = NE.map soPrice soList
        in PriceRange (minimum priceList) (maximum priceList)

showExecutionResult :: ExecutionResult -> String
showExecutionResult ExecutionResult{..}
    | Nothing <- liLiquidityInfo = unlines $
        logHeader
        ++
        [ "NO ORDERS MATCHED"
        , lineSeparator
        ]
    | Just LiquidityInfo{..} <- liLiquidityInfo
    , (_, quoteCurrency) <- liBaseQuote = unlines $
        logHeader
        ++
        [ logLine "buy liquidity" $ showAmount quoteCurrency (liquidity liBuyLiquidity)
        , logLine "sell liquidity" $ showAmount quoteCurrency (liquidity liSellLiquidity)
        , logLine "SUM" $ showAmount quoteCurrency (liquidity liBuyLiquidity + liquidity liSellLiquidity)
        ]
        ++
        catMaybes
            [ fmap (logLine "Buy price (low/high)" . showPriceRange) (liPriceRange <$> liBuyLiquidity)
            , fmap (logLine "Sell price (low/high)" . showPriceRange) (liPriceRange <$> liSellLiquidity)
            ]
        ++
        [ lineSeparator
        , "Buy paths:"
        , ""
        , maybe "" (showPaths quoteCurrency) (liPaths <$> liBuyLiquidity)
        , lineSeparator
        , "Sell paths:"
        , ""
        , maybe "" (showPaths quoteCurrency) (liPaths <$> liSellLiquidity)
        ]
        ++
        [ lineSeparator ]
  where
    showPaths quoteCurrency paths =
        unlines . NE.toList $ NE.map (pathSumRange quoteCurrency) paths
    liquidity = fromMaybe 0 . fmap liLiquidity
    showFloatSamePrecision num  = printf (printf "%%.%df" $ digitsAfterPeriod num) num
    digitsAfterPeriod num =
        let beforeRemoved = dropWhile (/= '.') $ printf "%f" num
        in if null beforeRemoved then 0 else length beforeRemoved - 1
    pathSumRange quoteCurrency (quoteAmount, priceRange, venue) =
        unlines
            [ logLine ("Volume (quote)") (showAmount quoteCurrency quoteAmount)
            , logLine "Price (low/high)" (showPriceRange priceRange)
            , logLine "Path" (toS venue)
            ]
    showPriceRange :: Real a => PriceRange a -> String
    showPriceRange PriceRange{..} = printf "%s / %s" (showPrice lowestPrice) (showPrice highestPrice)
    thousandSeparator numStr =
        let addDelimiter (index, char) accum =
                if index /= 0 && index `mod` (3 :: Int) == 0
                    then ',' : char : accum
                    else char : accum
        in reverse $ foldr addDelimiter [] (zip [0..] (reverse numStr))
    showInteger :: Lib.NumType -> String
    showInteger = thousandSeparator . show @Integer . floor
    showAmount :: Lib.Currency -> Lib.NumType -> String
    showAmount currency = (++ " " ++ toS currency) . showInteger
    showPrice :: Real price => price -> String
    showPrice = Format.formatFloatFloor 8
    lineSeparator = "-----------------------------------------------------"
    logHeader = [ lineSeparator, logInputFile, logLine "Cryptocurrency" (toS liCrypto), logMaxSlippage ]
    logInputFile = logLine "Order book file" liInputFile
    logMaxSlippage = logLine "Maximum slippage (%)" (showFloatSamePrecision liMaxSlippage)
    logLine :: String -> String -> String
    logLine title message =
            printf "%-25s%s" title message

visualize :: Opt.Options -> Lib.Currency -> FilePath -> Execution numType -> IO ()
visualize options currency outputDir Execution{..} =
    mainRun inputData >>= writeChartFile options outFilePath
  where
    mkOutFileName path = FP.takeBaseName path <> "-" <> toS currency <> FP.takeExtension path
    outFilePath = outputDir </> mkOutFileName inputFile


-- |
benchmark
    :: NFData numType
    => Maybe FilePath   -- ^ Write results to CSV file?
    -> Execution numType
    -> IO ()
benchmark csvFileM Execution{..} = do
    benchmark' <- benchSingle inputFile graphInfo inputData (void . mainRun)
    Criterion.runMode mode [benchmark']
  where
    mode = Criterion.Run config Criterion.Prefix [""]
    config = Criterion.defaultConfig { Criterion.csvFile = csvFileM }

-- |
benchSingle
    :: NFData numType
    => FilePath                     -- ^ Order book input file name
    -> GraphInfo numType
    -> [OrderBook numType]
    -> ([OrderBook numType] -> IO ())   -- ^ Run algorithm
    -> IO Criterion.Benchmark
benchSingle obFile GraphInfo{..} orderBooks action = do
    let name = obFile ++ " V=" ++ show (length giVertices) ++ " E=" ++ show giEdgeCount
    return $ Criterion.bench name $
        Criterion.perBatchEnv (const $ return orderBooks) action

-- |
withBidsAsksOrder
    :: Lib.Currency -- ^ Numeraire
    -> Lib.Currency -- ^ Cryptocurrency
       -- | arg1: buy order, arg2: sell order
    -> (forall src dst. (KnownSymbol src, KnownSymbol dst) => Lib.BuyOrder dst src
                                                           -> Lib.BuyOrder src dst
                                                           -> r
       )
    -> r
withBidsAsksOrder numeraire crypto f =
    case someSymbolVal (toS numeraire) of
        SomeSymbol (Proxy :: Proxy numeraire) ->
            case someSymbolVal (toS crypto) of
                SomeSymbol (Proxy :: Proxy crypto) ->
                    f (buyOrder :: Lib.BuyOrder crypto numeraire)
                      (buyOrder :: Lib.BuyOrder numeraire crypto)
  where
    buyOrder = Lib.unlimited

readOrdersFile
    :: (Json.FromJSON numType, Fractional numType, Real numType)
    => Opt.Options
    -> FilePath
    -> IO [OrderBook numType]
readOrdersFile options filePath = do
    log $ "Reading order books from " ++ show filePath ++ "..."
    books <- decodeFileOrFail filePath
    -- Log venues
    log ("Venues:") >> logVenues (nub $ map Book.bookVenue books)
    let orders = concatMap Book.fromOrderBook books
    log $ "Order book count: " ++ show (length books)
    log $ "Order count: " ++ show (length orders)
    -- TODO: print warning in case of input orderbook depth < 'maxSlippage'
    return $ map (Book.trimSlippageOB maxSlippage) books
  where
    log = Opt.logger options
    maxSlippage = toRational $ Opt.maxSlippage options
    throwError file str = error $ file ++ ": " ++ str
    decodeFileOrFail :: (Json.FromJSON numType, Ord numType) => FilePath -> IO [OrderBook numType]
    decodeFileOrFail file =
        either (throwError file) return =<< Json.eitherDecodeFileStrict file
    logVenues venues = forM_ venues $ \venue -> log ("\t" ++ toS venue)

buildGraph
    :: (Real numType)
    => [OrderBook numType]                         -- ^ Sell orders
    -> ST s (GraphInfo numType2, Lib.SellOrderGraph s g "arb")
buildGraph sellOrders = do
    graph <- Lib.build sellOrders
    currencies <- DG.vertexLabels graph
    edgeCount <- DG.edgeCount graph
    let gi = GraphInfo
                { giVertices    = currencies
                , giEdgeCount   = edgeCount
                }
    return (gi, graph)

-- NB: Phantom 'numType' is number type of input order book
data GraphInfo numType = GraphInfo
    { giVertices    :: [Lib.Currency]
    , giEdgeCount   :: Word
    }

matchOrders
    :: (KnownSymbol src, KnownSymbol dst, Real numType)
    => Opt.Options
    -> Lib.BuyOrder dst src     -- ^ Buy cryptocurrency for national currency
    -> Lib.BuyOrder src dst     -- ^ Sell cryptocurrency for national currency
    -> [OrderBook numType]      -- ^ Input orders
    -> IO ([SomeSellOrder], [SomeSellOrder])    -- ^ (bids, asks)
matchOrders options buyOrder sellOrder sellOrders =
    let log :: Monad m => String -> m ()
        log = Opt.logger options in
    ST.stToIO $ do
        log "Building graph..."
        (graphInfo, mGraph) <- buildGraph sellOrders
        let vertexCount = length (giVertices graphInfo)
            edgeCount = giEdgeCount graphInfo
        log $ "Vertex count: " ++ show vertexCount
        log $ "Edge count:   " ++ show edgeCount
        -- Arbitrages
        buyGraph <- Lib.runArb mGraph $ do
            log "Finding arbitrages..."
            -- Asks
            (_, arbsSell) <- Lib.arbitrages sellOrder
            (buyGraph, arbsBuy) <- Lib.arbitrages buyOrder
            let arbs = arbsSell ++ arbsBuy
            -- Finds all arbitrages (regardless of "src" vertex)
            log $ unlines ["Arbitrages:", pp arbs]
            return buyGraph
        -- Match
        Lib.runMatch buyGraph $ do
            log "Matching sell order..."
            bids <- map Lib.invertSomeSellOrder <$> Lib.match sellOrder
            log "Matching buy order..."
            asks <- Lib.match buyOrder
            return (bids, asks)

writeChartFile
    :: Opt.Options
    -> FilePath
    -> ([SomeSellOrder], [SomeSellOrder])
    -> IO ()
writeChartFile options obPath (bids, asks) = do
    log "Writing order book.."
    let trimmedAsks = trimOrders $ sortBy (comparing soPrice)        asks
        trimmedBids = trimOrders $ sortBy (flip $ comparing soPrice) bids
    Json.encodeFile obPath (mkJsonOb trimmedBids trimmedAsks)
    putStrLn $ "Wrote " ++ show obPath
  where
    trimOrders :: [SomeSellOrder] -> [SomeSellOrder]
    trimOrders = Util.compress 500 . Util.merge
    log = Opt.logger options

-- | Write JSON order book
mkJsonOb
    :: [SomeSellOrder]  -- ^ Bids
    -> [SomeSellOrder]  -- ^ Asks
    -> Json.Value
mkJsonOb bids asks =
    Json.object
        [ toS "bids" .= map toJson bids
        , toS "asks" .= map toJson asks
        ]
  where
    toJson :: SomeSellOrder -> Json.Value
    toJson sso = Json.toJSON -- format: ["0.03389994", 34.14155996]
        ( show (realToFrac $ soPrice sso :: Double)
        , realToFrac $ soQty sso :: Double
        )
