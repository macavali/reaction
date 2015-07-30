module Main where

import Options.Applicative
import Prelude hiding(readFile)
import Data.Text.IO(readFile)
import Flow.Kappa

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
  foldl (>>) (putStr "") $ map (putStrLn . show) (parseKappa s)
       
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
  
