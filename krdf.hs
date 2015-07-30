module Main where

import Options.Applicative
import Prelude hiding(readFile)
import Data.Text(unpack)
import Data.Text.IO(readFile, hPutStr)
import Flow.Kappa
import Flow.Kappa.Rdf
import System.IO(stdout)
import Swish.RDF.Formatter.Turtle(formatGraphAsText)

data Config = Cfg { filename :: String
                  , namespace :: String
                  }
args :: Parser Config
args = Cfg
       <$> strOption
         ( short 'f'
           <> metavar "FILENAME"
           <> help "Kappa file to read" )
       <*> strOption
         ( short 'n'
           <> metavar "NAMESPACE"
           <> help "RDF namespace to use" )
         
exec :: Config -> IO ()
exec (Cfg { filename }) = do
  s <- readFile filename
  hPutStr stdout $ formatGraphAsText $ annotations $ parseKappa s
--  foldl (>>) (putStr "") $ map (putStrLn . show) (annotations $ parseKappa s)
       
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
  
