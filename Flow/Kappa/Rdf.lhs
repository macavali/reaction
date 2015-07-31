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
import qualified Data.HashMap.Lazy as H
import qualified Data.Set as S
import Data.Text(Text, concat, append, pack, unpack)
import Data.Text.Lazy(fromStrict)
import Flow.Kappa(Statement(..), Rule(..), AgentP(..), SiteP, LinkP(..), StateP(..))
import Flow.Kappa.Vocabulary
import qualified Swish.RDF as RDF
import Swish.Namespace(makeScopedName)
import Swish.QName(newLName)
import Swish.RDF.Graph(RDFGraph, RDFLabel(..), ToRDFLabel(..), arc, addArc, namespaces, newNode)
import Swish.RDF.Parser.Turtle(parseTurtlefromText)
import Swish.RDF.Vocabulary.DublinCore
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

-- | Materialise information latent in the Kappa representation into RDF
-- | TODO: possibly handle statement types other than rule declarations
materialise :: [Statement] -> RDFGraph -> RDFGraph
materialise [] g = g
materialise (RD rule:rest) g = materialise rest (ruleToRDF rule g)
materialise (_:rest)       g = materialise rest g

-- | Produce RDF statements about a rule, and add them to the graph
-- | TODO: we should also materialise the rate expression, but for
-- |       now just leave a dangling blank node...
ruleToRDF :: Rule -> RDFGraph -> RDFGraph
ruleToRDF Rule { desc, lhs, rhs } g = rhsg
  where
    nrule     = lname g desc  -- rdf resource for the rule name
    brule     = newBnode g "rule" -- bnode as base for bindings
    nrate     = newBnode g "rate" -- bnode for rate
    nlhs      = newBnode g "lhs"  -- bnode for lhs
    nrhs      = newBnode g "rhs"  -- bnode for rhs
    (alhs, _) = lhs -- a half-rule contains agents and tokens
    (arhs, _) = rhs -- ditto
    -- the top level statements about the rule. minimally, its
    -- type, and linkage for rate expression and lhs, rhs parts
    triples   = [ arc nrule RDF.resRdfType (Res rbmoRule)
                , arc nrule (Res rbmoRate) nrate
                , arc nrule (Res rbmoLhs) nlhs
                , arc nrule (Res rbmoRhs) nrhs
                ]
    ruleg     = foldl raddArc g triples
    lhsg      = agentPats alhs brule nlhs ruleg
    rhsg      = agentPats arhs brule nrhs lhsg

-- | Simple tail-recursive utility to iterate through agent patterns
agentPats :: [AgentP] -> RDFLabel -> RDFLabel -> RDFGraph -> RDFGraph
agentPats (ap:as) b a g = agentPatToRDF ap b a nextg
  where nextg = agentPats as b a g
agentPats []      _ _ g = g

-- | Produce RDF statements about an agent pattern
-- | The arguments are, the agent itself, a base blank node
-- | from which bindings are to be generated, the anchor
-- | referring to the rule's right/left side, and the graph.
agentPatToRDF :: AgentP -> RDFLabel -> RDFLabel -> RDFGraph -> RDFGraph
agentPatToRDF (AgentP name sites) b anchor g = siteg
  where
    npat    = newBnode g "pat"
    triples = [ arc anchor (Res rbmoPatP) npat
              , arc npat RDF.resRdfType (Res rbmoPat)
              , arc npat (Res rbmoAgentP) (lname g name)
              ]
    ag      = foldl raddArc g triples
    siteg   = sitePats (H.toList sites) b npat ag

-- | Simple tail-recursive utility to iterate through site patterns
sitePats :: [(Text, SiteP)] -> RDFLabel -> RDFLabel -> RDFGraph -> RDFGraph
sitePats (s:ss) b a g = sitePatToRDF s b a nextg
  where nextg = sitePats ss b a g
sitePats []     _ _ g = g

-- | Produce RDF statements about a site patern
-- | The arguments are similar to `agentPatToRDF` although in
-- | this case, the anchor refers to the agent pattern.
sitePatToRDF :: (Text, SiteP) -> RDFLabel -> RDFLabel -> RDFGraph -> RDFGraph
sitePatToRDF (name, (link, state)) (Blank b) anchor g = siteg
  where
    nsite  = newBnode g "site"
    -- bound state, construct a blank node from the given blank
    -- node b to ensure consistent naming for "rule scope"
    linkState (Link l) =
      [ arc nsite (Res rbmoBindingP) (Blank $ b ++ "_" ++ unpack l) ]
    -- when we know the site is bound, but we do not know to what
    -- use a new blank node
    linkState Bound =
      [ arc nsite (Res rbmoBindingP) (newBnode g "binding") ]
    -- unbound and maybebound use special rbmo terms
    linkState Unbound =
      [ arc nsite (Res rbmoBindingP) (Res rbmoNothing) ]
    linkState MaybeBound =
      [ arc nsite (Res rbmoBindingP) (Res rbmoUnknown) ]
    -- internal state
    iState (State s) =
      [ arc nsite (Res rbmoIntP) (toRDFLabel $ unpack s) ]
    iState _    = []
    triples = [ arc anchor (Res rbmoSiteP) nsite
              , arc nsite RDF.resRdfType (Res rbmoState)
              , arc nsite (Res dctidentifier) (lname g name)
              ] ++ linkState link ++ iState state
    siteg = foldl raddArc g triples
sitePatToRDF _ _ _ _ = undefined
\end{code}

\hide{
\begin{code}
{- Various utility functions -}

-- | Construct a local name from the RDF graph's empty namespace
-- | (which is assumed to exist and have been declared). This is
-- | used to construct references to rules and agents defined in
-- | the Kappa file and, presumably, annotated
lname :: RDFGraph -> Text -> RDFLabel
lname g n = Res $ makeScopedName Nothing base name
  where 
    name = case newLName n of
      Just ln -> ln
      Nothing -> error $ "Local name '" ++ (unpack n) ++ "' is not valid"
    base = case M.lookup Nothing $ namespaces g of
      Just u  -> u
      Nothing -> error "The empty prefix (:) is not declared in the source graph"

-- | Get a new, fresh bnode based on the given string, unique for
-- | the graph
newBnode :: RDFGraph -> String -> RDFLabel
newBnode g n = newNode (Blank n) existing
  where
    existing = S.toList $ RDF.allLabels RDF.isBlank g

-- | A version of addArc with arguments reversed suited to fold
raddArc :: RDF.Label lb => RDF.NSGraph lb -> RDF.Arc lb -> RDF.NSGraph lb
raddArc t a = addArc a t
\end{code}
}

% Local Variables:
% compile-command: "cd ../..; cabal build; cabal test"
% End:
