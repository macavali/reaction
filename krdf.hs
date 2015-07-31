module Main where

import Prelude hiding(readFile)
import Data.Text.IO(readFile, hPutStr)
import Flow.Kappa
import Flow.Kappa.Rdf
import Options.Applicative
import System.IO(stdout)
import Swish.RDF(RDFGraph, emptyRDFGraph, getNamespaces, setNamespaces)
import Swish.RDF.Formatter.Turtle(formatGraphAsText)

data Config = Cfg { filename :: String
                  , doAnnotate    :: Bool
                  , doMaterialise :: Bool
                  , doNormalise   :: Bool
                  }
args :: Parser Config
args = Cfg
       <$> strOption
       ( short 'f'
         <> long "filename"
         <> metavar "FILENAME"
         <> help "Kappa file to read" )
       <*> switch
       ( short 'a'
         <> long "annotations"
         <> help "Extract annotations" )
       <*> switch
       ( short 'm'
         <> long "materialise"
         <> help "Materialise Kappa statements to RDF" )
       <*> switch
       ( short 'n'
         <> long "normalise"
         <> help "Normalise agent patterns according to declarations" )

genRdf :: Config -> [Statement] -> RDFGraph
genRdf cfg kappa = materialised
  where
    normalised
      | doNormalise cfg   = normalise kappa
      | otherwise         = kappa
    annotated
      | doAnnotate cfg    = annotations kappa
      | otherwise         = emptyGraph
    materialised
      | doMaterialise cfg = materialise normalised annotated
      | otherwise         = annotated

    -- a few hoops to jump through to get an empty graph
    -- with the right namespace
    nsmap      = getNamespaces $ annotations kappa
    emptyGraph = setNamespaces nsmap emptyRDFGraph
    
readKappaWriteRDF :: Config -> IO ()
readKappaWriteRDF cfg = do
  input <- readFile $ filename cfg
  hPutStr stdout $ formatGraphAsText $ genRdf cfg $ parseKappa input

main :: IO ()
main = do
  execParser opts >>= readKappaWriteRDF
  where
    opts = info (helper <*> args)
      ( fullDesc
     <> progDesc "Transform Kappa rules to RDF"
     <> header "Kappa -> RDF" )
           
-- Local Variables:
-- compile-command: "cabal build"
-- End:
  
