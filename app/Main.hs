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
import qualified OrderBook.Graph                            as Lib

import qualified Control.Monad.ST                           as ST
import           Data.List                                  (sortBy, (\\))
import           Data.Ord                                   (comparing)

import qualified Data.List.NonEmpty                         as NE
import qualified Data.Aeson                                 as Json
import           Data.Aeson                                 ((.=))
import           System.FilePath                            ((</>))
import qualified System.FilePath                            as FP
import qualified Criterion
import qualified Criterion.Main                             as Criterion
import qualified Criterion.Main.Options                     as Criterion
import qualified Criterion.Types                            as Criterion
import qualified UnliftIO.Async                             as Async
import qualified Data.Csv.Incremental                       as Csv
-- TMP TEST
import Control.Exception (SomeException(SomeException), catch, throwIO)


printErr :: SomeException -> IO a
printErr se@(SomeException e) = putStrLn ("caught: " ++ show e) >> throwIO se

main :: IO ()
main = Opt.withOptions $ \options ->
    Opt.withNumberType options $ \(Opt.SomeNumberType (_ :: Proxy numType)) ->
    forM_ (Opt.inputFiles options) $ \inputFile -> do
        orderBooks :: [OrderBook numType] <- Lib.readOrdersFile
            (Opt.logger options) (toRational $ Opt.maxSlippage options) inputFile
        (graphInfo, graph) <- Lib.buildBuyGraph (Opt.logger options) orderBooks
        let executionCryptoList = mkExecutions options graphInfo inputFile graph
        logResult <- forAll (Opt.mode options) executionCryptoList $ \(execution, crypto) -> do
            case Opt.mode options of
                    Opt.Analyze ->
                        (Just . showExecutionResult options) <$> analyze crypto options execution
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
        let liquidity sideM = fromMaybe 0 $ Lib.liLiquidity <$> (liLiquidityInfo er >>= sideM)
        in csvOutput er (liquidity Lib.liBuyLiquidity) (liquidity Lib.liSellLiquidity)
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
    , graphInfo     :: Lib.GraphInfo numType
      -- ^ Information about the graph
    , inputData     :: Lib.IBuyGraph
      -- ^ Graph without arbitrages. Built from order books read from 'inputFile'.
    , mainRun       :: Lib.IBuyGraph -> IO ([Lib.SellPath], [Lib.BuyPath])
      -- ^ Process input data
    }

mkExecutions
    :: (Json.FromJSON numType, Fractional numType, Real numType)
    => Opt.Options
    -> Lib.GraphInfo numType
    -> FilePath
    -> Lib.IBuyGraph
    -> [(Execution numType, Lib.Currency)]
mkExecutions options graphInfo inputFile graph = do
    map (\crypto -> (mkExecution crypto, crypto)) allCryptos
  where
    allCryptos = case Opt.crypto options of
            Opt.OneOrMore cryptos -> NE.toList cryptos
            Opt.AllCryptos    -> Lib.giVertices graphInfo \\ [numeraire]
    mkExecution crypto =
        Execution inputFile graphInfo graph (mainRun crypto)
    mainRun crypto orders = ST.stToIO $
        Lib.matchOrders (Opt.logger options) numeraire crypto orders
    numeraire   = Opt.numeraire options

-- | Result for an entire execution
data ExecutionResult = ExecutionResult
    { liInputFile       :: FilePath
    , liMaxSlippage     :: Double
    , liCrypto          :: Lib.Currency     -- ^ Target cryptocurrency
    , liNumeraire       :: Lib.Currency     -- ^ Numeraire
    , liLiquidityInfo   :: Maybe Lib.LiquidityInfo
    }

analyze :: Lib.Currency -> Opt.Options -> Execution numType -> IO ExecutionResult
analyze cryptocurrency Opt.Options{..} Execution{..} = do
    (sellPath, buyPath) <- mainRun inputData
    return $ ExecutionResult
        { liInputFile       = inputFile
        , liMaxSlippage     = maxSlippage
        , liCrypto          = cryptocurrency
        , liNumeraire       = numeraire
        , liLiquidityInfo   = Lib.toLiquidityInfo (sellPath, buyPath)
        }

showExecutionResult :: Opt.Options -> ExecutionResult -> String
showExecutionResult Opt.Options{..} ExecutionResult{..}
    | Nothing <- liLiquidityInfo = unlines $
        logHeader
        ++
        [ "NO ORDERS MATCHED"
        , lineSeparator
        ]
    | Just Lib.LiquidityInfo{..} <- liLiquidityInfo = unlines $
        logHeader
        ++
        [ logLine "buy liquidity" $ showAmount (liquidity liBuyLiquidity)
        , logLine "sell liquidity" $ showAmount (liquidity liSellLiquidity)
        , logLine "SUM" $ showAmount (liquidity liBuyLiquidity + liquidity liSellLiquidity)
        , logLine "Buy price (low/high)"  $ maybe "-" (showPriceRange) (Lib.liPriceRange <$> liBuyLiquidity)
        , logLine "Sell price (low/high)" $ maybe "-" (showPriceRange) (Lib.liPriceRange <$> liSellLiquidity)
        , lineSeparator
        ]
        ++
        if maxNumPaths > 0
            then prettyPrintPaths "Buy" liBuyLiquidity
                 ++ prettyPrintPaths "Sell" liSellLiquidity
            else []
  where
    prettyPrintPaths strSide liSide =
        [ strSide <> " paths:", "" , maybe "<no paths>" showPaths (Lib.liPaths <$> liSide), lineSeparator ]
    showPaths paths =
        unlines $ map pathSumRange (NE.take maxNumPaths paths)
    liquidity = fromMaybe 0 . fmap Lib.liLiquidity
    showFloatSamePrecision num  = printf (printf "%%.%df" $ digitsAfterPeriod num) num
    digitsAfterPeriod num =
        let beforeRemoved = dropWhile (/= '.') $ printf "%f" num
        in if null beforeRemoved then 0 else length beforeRemoved - 1
    pathSumRange (quoteAmount, priceRange, path) =
        unlines
            [ logLine ("Volume (quote)") (showAmount quoteAmount)
            , logLine "Price (low/high)" (showPriceRange priceRange)
            , logLine "Path" (toS $ Lib.showPath path)
            ]
    showPriceRange :: Real a => Lib.PriceRange a -> String
    showPriceRange Lib.PriceRange{..} = printf "%s / %s" (showPrice lowestPrice) (showPrice highestPrice)
    thousandSeparator numStr =
        let addDelimiter (index, char) accum =
                if index /= 0 && index `mod` (3 :: Int) == 0
                    then ',' : char : accum
                    else char : accum
        in reverse $ foldr addDelimiter [] (zip [0..] (reverse numStr))
    showInteger :: Lib.NumType -> String
    showInteger = thousandSeparator . show @Integer . floor
    showAmount :: Lib.NumType -> String
    showAmount = (++ " " ++ toS liNumeraire) . showInteger
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
    -> Lib.GraphInfo numType
    -> Lib.IBuyGraph
    -> (Lib.IBuyGraph -> IO ())   -- ^ Run algorithm
    -> IO Criterion.Benchmark
benchSingle obFile Lib.GraphInfo{..} graph action = do
    let name = obFile ++ " V=" ++ show (length giVertices) ++ " E=" ++ show giEdgeCount
    return $ Criterion.bench name $
        Criterion.perBatchEnv (const $ return graph) action

writeChartFile
    :: Opt.Options
    -> FilePath
    -> ([Lib.SellPath], [Lib.BuyPath])
    -> IO ()
writeChartFile options obPath (sellPaths, buyPaths) = do
    log "Writing order book.."
    let trimmedAsks = trimOrders $ sortBy (comparing soPrice)        asks
        trimmedBids = trimOrders $ sortBy (flip $ comparing soPrice) bids
        (bids, asks) = (map toFakeSellOrder sellPaths, map Lib.toSellOrder buyPaths)
    Json.encodeFile obPath (mkJsonOb trimmedBids trimmedAsks)
    putStrLn $ "Wrote " ++ show obPath
  where
    -- convert to a sell order that's really a buy order.
    -- used to be compatible with 'mkJsonOb'.
    -- works because 'mkJsonOb' doesn't care about base/quote symbol.
    toFakeSellOrder :: Lib.SellPath -> SomeSellOrder
    toFakeSellOrder = Lib.invertSomeSellOrder . Lib.toSellOrder
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
