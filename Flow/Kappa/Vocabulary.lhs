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
  , rbmoAgent
  , rbmoAgentP
  , rbmoSite
  , rbmoSiteP
  , rbmoBindingP
  , rbmoIntP
  , rbmoNothing
  , rbmoUnknown
  , rbmoHasSite
  , rbmoHasConfiguration
  , rbmoConfigurationOf
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

rbmoAgentP :: ScopedName
rbmoAgentP = toR "agent"

rbmoAgent :: ScopedName
rbmoAgent = toR "Agent"

rbmoSiteP :: ScopedName
rbmoSiteP = toR "site"

rbmoSite :: ScopedName
rbmoSite = toR "Site"

rbmoBindingP :: ScopedName
rbmoBindingP = toR "binding"

rbmoIntP :: ScopedName
rbmoIntP = toR "internal"

rbmoNothing :: ScopedName
rbmoNothing = toR "nothing"

rbmoUnknown :: ScopedName
rbmoUnknown = toR "unknown"

rbmoHasSite :: ScopedName
rbmoHasSite = toR "hasSite"

rbmoHasConfiguration :: ScopedName
rbmoHasConfiguration = toR "hasConfiguration"

rbmoConfigurationOf :: ScopedName
rbmoConfigurationOf = toR "configurationOf"
\end{code}

% Local Variables:
% compile-command: "cd ../..; cabal build; cabal test"
% End:
