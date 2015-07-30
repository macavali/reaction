\hide{
\begin{code}
module Flow.Kappa.Rdf (
  annotations
  , turtle
  ) where

import Prelude hiding (concat)
import Data.Text(Text, concat, append, pack)
import Data.Text.Lazy(fromStrict)
import Flow.Kappa(Statement(..))

import Swish.RDF.Graph(RDFGraph)
import Swish.RDF.Parser.Turtle(parseTurtlefromText)

-- | Extract the explicit RDF/turtle annotations from a list of statements
turtle :: [Statement] -> Text
turtle = concat . map onlyText . filter isRdf
  where
    isRdf (RDF _)    = True
    isRdf _          = False
    onlyText (RDF t) = append t (pack "\n")
    onlyText _       = undefined

-- | Translate the explicit RDF/turtle annotations into an RDF graph
annotations :: [Statement] -> RDFGraph
annotations s = case parseTurtlefromText (fromStrict $ turtle s) of
  Left err -> error err
  Right g  -> g

\end{code}
}

% Local Variables:
% compile-command: "cd ../..; cabal build; cabal test"
% End:
