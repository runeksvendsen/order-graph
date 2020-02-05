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

import           OrderBook.Graph.Internal.Prelude           hiding (log)
import           Options.Applicative
import qualified Data.List.NonEmpty                         as NE
import qualified Control.Logging                            as Log
import           Data.Char                                  (toLower)

import qualified OrderBook.Graph                            as Lib
import qualified Data.Aeson                                 as Json


data Options = Options
  { inputFiles  :: NE.NonEmpty FilePath
  , maxSlippage :: Word
  , numeraire   :: Lib.Currency
  , crypto      :: Crypto
  , mode        :: Mode
  , logLevel    :: Log.LogLevel
  , numberType  :: SomeNumberType
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
      <*> fmap toSomeNumberType numberTypeOpt

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
      -- ^ Same as 'Benchmark' but also results to CSV file
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
analyzeCsvOpt = flag' AnalyzeCsv
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
