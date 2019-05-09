module Options
( withOptions
, Options(..)
, Crypto(..)
, Mode(..)
)
where

import           OrderBook.Graph.Internal.Prelude           hiding (log)
import           Options.Applicative
import qualified Data.List.NonEmpty                         as NE
import qualified Control.Logging                            as Log

import qualified OrderBook.Graph                            as Lib


data Options = Options
  { inputFiles  :: NE.NonEmpty FilePath
  , maxSlippage :: Word
  , numeraire   :: Lib.Currency
  , crypto      :: Crypto
  , mode        :: Mode
  , logLevel    :: Log.LogLevel
  } deriving (Eq, Show)

withOptions :: (Options -> IO ()) -> IO ()
withOptions f = do
    parsedOptions <- execParser opts
    setLogLevel parsedOptions
    Log.withStderrLogging (f parsedOptions)

opts :: ParserInfo Options
opts = info options $
     fullDesc
  <> progDesc "Analyze market depth algorithm"
  <> header "Match buy and sell orders"

options :: Parser Options
options = Options
      <$> inputFilesOpt
      <*> maxSlippageOpt
      <*> numeraireOpt
      <*> cryptoOpt
      <*> modeOpt
      <*> verboseOpt

-- | Represents either a single cryptocurrency or all cryptocurrencies
data Crypto
  = Single Lib.Currency
  | AllCryptos
    deriving (Eq, Show)

-- |
data Mode
    = Analyze
      -- ^ Print information about cryptocurrency liquidity
    | AnalyzeCsv FilePath
      -- ^ Write information about cryptocurrency liquidity to CSV file
    | Visualize FilePath
      -- ^ Write matched orders to orderbook file for visualization in a depth chart.
      --   Argument specifies orderbook output directory.
    | Benchmark
      -- ^ Run benchmark of order matching algorithm
    | BenchmarkCsv FilePath
      -- ^ Same as 'Benchmark' but also results to CSV file
      deriving (Eq, Show)

modeOpt :: Parser Mode
modeOpt = visualizeOpt <|> benchOpt <|> benchCsvOpt <|> analyzeOpt <|> analyzeCsvOpt

setLogLevel :: Options -> IO ()
setLogLevel = Log.setLogLevel . logLevel

visualizeOpt :: Parser Mode
visualizeOpt = Visualize <$> strOption
  (  long "visualize"
  <> short 'w'
  <> metavar "OUTDIR"
  <> help "Target directory for matched orders orderbook files" )

benchOpt :: Parser Mode
benchOpt = flag' Benchmark
  (  long "bench"
  <> help "Benchmark" )

benchCsvOpt :: Parser Mode
benchCsvOpt = BenchmarkCsv <$> strOption
  (  long "bench-csv"
  <> metavar "CSVFILE"
  <> help "Benchmark and also write results to CSV file" )

analyzeOpt :: Parser Mode
analyzeOpt = flag' Analyze
  (  long "analyze"
  <> help "Print information about the liquidity of the cryptocurrency" )

analyzeCsvOpt :: Parser Mode
analyzeCsvOpt = AnalyzeCsv <$> strOption
  (  long "analyze-csv"
  <> help "Write information about cryptocurrency liquidity to CSV file" )


inputFilesOpt :: Parser (NE.NonEmpty FilePath)
inputFilesOpt = fmap NE.fromList . some $ argument str
  ( metavar "FILE..."
  <> help "JSON order book file" )

maxSlippageOpt :: Parser Word
maxSlippageOpt = option auto
  ( long "max-slippage"
  <> short 's'
  <> value 50
  <> metavar "MAX_SLIPPAGE"
  <> help "Stop after this percentage slippage is reached" )

numeraireOpt :: Parser Lib.Currency
numeraireOpt = toS . uppercase <$> strOption
  ( long "numeraire"
  <> short 'n'
  <> value "USD"
  <> metavar "NUMERAIRE"
  <> help "Sell/buy crypto for this national currency (e.g. EUR, USD, GBP)" )

cryptoOpt :: Parser Crypto
cryptoOpt = cryptoOptSingle <|> cryptoOptAll

cryptoOptSingle :: Parser Crypto
cryptoOptSingle = Single . toS . uppercase <$> strOption
  ( long "crypto"
  <> short 'c'
  <> metavar "CRYPTOCURRENCY"
  <> help "Sell/buy this cryptocurrency (e.g. BTC, ETH)" )

cryptoOptAll :: Parser Crypto
cryptoOptAll = flag' AllCryptos
  (  long "all-cryptos"
  <> short 'a'
  <> help "Run for all cryptocurrencies instead of a single"
  )

verboseOpt :: Parser Log.LogLevel
verboseOpt = flag Log.LevelError Log.LevelDebug
  (  long "verbose"
  <> short 'v'
  <> help "Print all information"
  )
