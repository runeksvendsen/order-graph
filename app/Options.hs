module Options
( opts
, Options(..)
, Mode(..)
-- * Re-exports
, execParser
)
where

import           Options.Applicative
import qualified Data.List.NonEmpty             as NE


data Options = Options
  { inputFiles  :: NE.NonEmpty FilePath
  , maxSlippage :: Word
  , numeraire   :: String
  , crypto      :: String
  , mode        :: Mode
  } deriving (Eq, Show)

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

-- |
data Mode
    = Visualize FilePath
      -- ^ Write matched orders to orderbook file for visualization in a depth chart.
      --   Argument specifies orderbook output directory.
    | Benchmark
      -- ^ Run benchmark of order matching algorithm
      deriving (Eq, Show)

modeOpt :: Parser Mode
modeOpt = visualizeOpt <|> benchOpt

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

numeraireOpt :: Parser String
numeraireOpt = strOption
  ( long "numeraire"
  <> short 'n'
  <> value "USD"
  <> metavar "NUMERAIRE"
  <> help "Sell/buy crypto for this national currency (e.g. EUR, USD, GBP)" )


cryptoOpt :: Parser String
cryptoOpt = strOption
  ( long "crypto"
  <> short 'c'
  <> metavar "CRYPTOCURRENCY"
  <> help "Sell/buy this cryptocurrency (e.g. BTC, ETH)" )
