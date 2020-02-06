{-# LANGUAGE ExistentialQuantification #-}
module Options
( withOptions
, Options(..)
, Crypto(..)
, Mode(..)
, SomeNumberType(..)
, withNumberType
)
where

import           OrderBook.Graph.Internal.Prelude           hiding (log, Mod)
import           Options.Applicative
import qualified Data.List.NonEmpty                         as NE
import qualified Control.Logging                            as Log
import           Data.Char                                  (toLower)

import qualified OrderBook.Graph                            as Lib
import qualified Data.Aeson                                 as Json


data Options = Options
  { mode        :: Mode
  , maxSlippage :: Double
  , crypto      :: Crypto
  , numeraire   :: Lib.Currency
  , logLevel    :: Log.LogLevel
  , numberType  :: SomeNumberType
  , inputFiles  :: NE.NonEmpty FilePath
  }

withOptions :: (Options -> IO ()) -> IO ()
withOptions f = do
    parsedOptions <- execParser opts
    setLogLevel parsedOptions
    Log.withStderrLogging (f parsedOptions)

withNumberType :: Options -> (SomeNumberType -> IO ()) -> IO ()
withNumberType opt f =
    f (numberType opt)

opts :: ParserInfo Options
opts = info (helper <*> options) $
     fullDesc
  <> header "Analyze cryptocurrency market depth"

options :: Parser Options
options = Options
      <$> modeOpt
      <*> maxSlippageOpt
      <*> cryptoOpt
      <*> numeraireOpt
      <*> verboseOpt
      <*> fmap toSomeNumberType numberTypeOpt
      <*> inputFilesOpt

-- | Represents either a single cryptocurrency or all cryptocurrencies
data Crypto
  = Single Lib.Currency
  | AllCryptos
    deriving (Eq, Show)

-- |
data Mode
    = Analyze
      -- ^ Print information about cryptocurrency liquidity
    | AnalyzeCsv
      -- ^ Write information about cryptocurrency liquidity to CSV file
    | Visualize FilePath
      -- ^ Write matched orders to orderbook file for visualization in a depth chart.
      --   Argument specifies orderbook output directory.
    | Benchmark
      -- ^ Run benchmark of order matching algorithm
    | BenchmarkCsv FilePath
      -- ^ Same as 'Benchmark' but also write results to CSV file
      deriving (Eq, Show)

data SomeNumberType
    =  forall numType.
       ( Json.FromJSON numType
       , Real numType
       , Fractional numType
       , NFData numType
       )
    => SomeNumberType (Proxy numType)

modeOpt :: Parser Mode
modeOpt = hsubparser $
    analyzeCommand <> analyzeCsvCommand <> visualizeCommand <> benchCommand <> benchCsvCommand

setLogLevel :: Options -> IO ()
setLogLevel = Log.setLogLevel . logLevel

visualizeCommand :: Mod CommandFields Mode
visualizeCommand =
    command "visualize" $
        info visualizeOptions (progDesc "Write matched orders to orderbook file for visualization in a depth chart")
  where
    visualizeOptions :: Parser Mode
    visualizeOptions = Visualize <$> arg
    arg = strArgument $
           metavar "OUTDIR"
        <> help "Target directory for matched orders orderbook files"

benchCommand :: Mod CommandFields Mode
benchCommand =
    command "bench" $
        info (pure Benchmark) (progDesc "Benchmark the order matching algorithm")

benchCsvCommand :: Mod CommandFields Mode
benchCsvCommand =
    command "bench-csv" $
        info options' (progDesc "Benchmark and also write results to CSV file")
  where
    options' :: Parser Mode
    options' = BenchmarkCsv <$> arg
    arg = strArgument $
           metavar "CSVFILE"
        <> help "Target file for benchmark results (CSV format)"

analyzeCommand :: Mod CommandFields Mode
analyzeCommand =
    command "analyze" $
        info (pure Analyze) (progDesc "Print information about cryptocurrency liquidity")

analyzeCsvCommand :: Mod CommandFields Mode
analyzeCsvCommand =
    command "analyze-csv" $
        info (pure AnalyzeCsv) (progDesc "Analyze and print liquidity information in CSV format")

inputFilesOpt :: Parser (NE.NonEmpty FilePath)
inputFilesOpt = fmap NE.fromList . some $ argument str
  ( metavar "FILE..."
  <> help "JSON order book file(s)" )

maxSlippageOpt :: Parser Double
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

data NumberType
    = Rational -- ^ 'Rational' number type
    | Double   -- ^ 'Double' number type
      deriving (Eq, Show)

showLowerCase :: NumberType -> String
showLowerCase = map toLower . show

readLowerCase :: String -> Maybe NumberType
readLowerCase string
    | lowerCaseString == "rational" = Just Rational
    | lowerCaseString == "double" = Just Double
    | otherwise = Nothing
  where
    lowerCaseString = map toLower string

toSomeNumberType
    :: NumberType
    -> SomeNumberType
toSomeNumberType Rational = SomeNumberType (Proxy :: Proxy Rational)
toSomeNumberType Double = SomeNumberType (Proxy :: Proxy Double)

numberTypeOpt :: Parser NumberType
numberTypeOpt =
  option (maybeReader readLowerCase) $
     long "number-type"
  <> value Rational
  <> help "Number type for input JSON order book file(s)"
  <> showDefaultWith showLowerCase
  <> metavar "rational/double"
