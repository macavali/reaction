\hide{
\begin{code}
{-# LANGUAGE OverloadedStrings #-}
module Flow.Kappa.Vocabulary (
  rbmoURI
  , namespaceRBMO
  , rbmoRule
  , rbmoRate
  , rbmoLhs
  , rbmoRhs
  , rbmoPat
  , rbmoPatP
  , rbmoAgent
  , rbmoAgentP
  ) where

import Data.Maybe(fromMaybe)
import Network.URI(URI, parseURI)
import Swish.Namespace (Namespace, ScopedName, makeNamespace, makeNSScopedName)
import Swish.QName (LName)

\end{code}
}

\begin{code}
------------------------------------------------------------
--  Namespace
------------------------------------------------------------

rbmoURI :: URI
rbmoURI = fromMaybe (error "Internal error processing RBMO URI") $ parseURI
         "http://purl.org/rbm/rbmo/"

-- | Maps @rbmo@ to <http://purl.org/rbm/rbmo/>
namespaceRBMO :: Namespace
namespaceRBMO = makeNamespace (Just "rbmo") rbmoURI

------------------------------------------------------------
--  Terms
------------------------------------------------------------

toR :: LName -> ScopedName
toR = makeNSScopedName namespaceRBMO

rbmoRule :: ScopedName
rbmoRule = toR "Rule"

rbmoRate :: ScopedName
rbmoRate = toR "rate"

rbmoLhs :: ScopedName
rbmoLhs = toR "lhs"

rbmoRhs :: ScopedName
rbmoRhs = toR "rhs"

rbmoPatP :: ScopedName
rbmoPatP = toR "pattern"

rbmoPat :: ScopedName
rbmoPat = toR "Pattern"

rbmoAgentP :: ScopedName
rbmoAgentP = toR "agent"

rbmoAgent :: ScopedName
rbmoAgent = toR "Agent"

\end{code}

% Local Variables:
% compile-command: "cd ../..; cabal build; cabal test"
% End: