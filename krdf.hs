module Main where

import Prelude hiding(readFile)
import Data.Text.IO(readFile, hPutStr)
import Flow.Kappa
import Flow.Kappa.Rdf
import Options.Applicative
import System.IO(stdout)
import Swish.RDF.Formatter.Turtle(formatGraphAsText)

data Config = Cfg { filename :: String
                  }
args :: Parser Config
args = Cfg
       <$> strOption
         ( short 'f'
           <> metavar "FILENAME"
           <> help "Kappa file to read" )

exec :: Config -> IO ()
exec (Cfg { filename }) = do
  input <- readFile filename
  statements <- return $ normalise $ parseKappa input
  annotationGraph <- return $ annotations statements
  materialisedGraph <- return $ materialise statements annotationGraph
  hPutStr stdout $ formatGraphAsText materialisedGraph

main :: IO ()
main = do
  execParser opts >>= exec
  where
    opts = info (helper <*> args)
      ( fullDesc
     <> progDesc "Transform Kappa rules to RDF"
     <> header "Kappa -> RDF" )
           
-- Local Variables:
-- compile-command: "cabal build"
-- End:
  
