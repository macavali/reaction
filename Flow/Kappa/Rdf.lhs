\hide{
\begin{code}
{-# LANGUAGE NamedFieldPuns, OverloadedStrings #-}
module Flow.Kappa.Rdf (
  annotations
  , materialise
  , turtle
  ) where

import Prelude hiding (concat)
import qualified Data.Map as M
import qualified Data.List as L
import Data.Text(Text, concat, append, pack, unpack)
import Data.Text.Lazy(fromStrict)
import Flow.Kappa(Statement(..), Rule(..), AgentP(..))
import Flow.Kappa.Vocabulary
import Network.URI(URI)
import qualified Swish.RDF as RDF
import Swish.Namespace(makeScopedName)
import Swish.QName(newLName)
import Swish.RDF.Graph(RDFGraph, RDFLabel(..), arc, addArc, namespaces)
import Swish.RDF.Parser.Turtle(parseTurtlefromText)
\end{code}
}
\begin{code}

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
annotations statements =
  case parseTurtlefromText (fromStrict $ turtle statements) of
   Left err -> error err
   Right g  -> g

materialise :: [Statement] -> RDFGraph -> RDFGraph
materialise [] g = g
materialise (RD rule:rest) g = materialise rest (ruleToRDF rule g)
materialise (_:rest) g = materialise rest g

lname :: RDFGraph -> Text -> RDFLabel
lname g n = Res $ makeScopedName Nothing (base g) name
  where 
    name = case newLName n of
      Just ln -> ln
      Nothing -> error $ "Local name '" ++ (unpack n) ++ "' is not valid"

base :: RDFGraph -> URI
base g = case M.lookup Nothing $ namespaces g of
  Just u  -> u
  Nothing -> error "The empty prefix (:) is not declared in the source graph"

ruleToRDF :: Rule -> RDFGraph -> RDFGraph
ruleToRDF Rule { desc, lhs, rhs } g = foldl raddArc g triples
  where
    raddArc t a = addArc a t
    nrule    = lname g desc
    nrate    = Blank $ (unpack desc) ++ "_rate"
    nlhs     = Blank $ (unpack desc) ++ "_lhs"
    nrhs     = Blank $ (unpack desc) ++ "_rhs"
    (alhs, _) = lhs
    (arhs, _) = rhs
    triples = [ arc nrule RDF.resRdfType (Res rbmoRule)
              , arc nrule (Res rbmoRate) nrate
              , arc nrule (Res rbmoLhs) nlhs
              , arc nrule (Res rbmoRhs) nrhs
              ] ++ lhsTriples ++ rhsTriples
    lhsTriples = L.concat (map (agentPat g (unpack desc ++ "_lhs") nlhs) alhs)
    rhsTriples = L.concat (map (agentPat g (unpack desc ++ "_rhs") nrhs) arhs)
              

agentPat :: RDFGraph -> String -> RDFLabel -> AgentP -> [RDF.Arc RDFLabel]
agentPat g bnbase anchor (AgentP name _) =
  [ arc anchor (Res rbmoPatP) npat
  , arc npat RDF.resRdfType (Res rbmoPat)
  , arc npat (Res rbmoAgentP) (lname g name)
  ]
  where
    npat = Blank $ bnbase ++ "_" ++ (unpack name)
\end{code}
}

% Local Variables:
% compile-command: "cd ../..; cabal build; cabal test"
% End:
