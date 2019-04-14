module Options
( opts
, Options(..)
-- * Re-exports
, execParser
)
where

import           Options.Applicative
import qualified Data.List.NonEmpty             as NE


opts :: ParserInfo Options
opts = info options $
     fullDesc
  <> progDesc "Analyze market depth algorithm"
  <> header "Match buy and sell orders"

data Options = Options
  { inputFiles  :: NE.NonEmpty FilePath
  , outputDir   :: FilePath
  , maxSlippage :: Word
  , numeraire   :: String
  , crypto      :: String
  } deriving (Eq, Show)

options :: Parser Options
options = Options
      <$> inputFilesOpt
      <*> outputDirOpt
      <*> maxSlippageOpt
      <*> numeraireOpt
      <*> cryptoOpt

inputFilesOpt :: Parser (NE.NonEmpty FilePath)
inputFilesOpt = fmap NE.fromList . some $ argument str
  ( metavar "FILE..."
  <> help "JSON order book file" )

outputDirOpt :: Parser FilePath
outputDirOpt = strOption
  (  long "out-dir"
  <> metavar "OUTDIR"
  <> help "Directory to write matched orders to" )


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
