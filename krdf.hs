module Main where

import Prelude hiding(readFile)
import Data.Attoparsec.Text(parseOnly)
import Data.Text.IO(readFile)
import Flow.Internal.KappaParser(kappaParser)

main :: IO ()
main = do
       k <- readFile "test.kappa"
       case parseOnly kappaParser k of
        Right r  -> foldl (>>) (putStr "") $ map (putStrLn . show) r
        Left err -> error err

-- Local Variables:
-- compile-command: "cabal build"
-- End:
  
