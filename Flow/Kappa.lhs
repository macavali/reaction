\hide{
\begin{code}
{-
    Reaction Networks -- Kappa
    Copyright (C) 2014,2015 William Waites

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.
-}
module Flow.Kappa (
  kappaParser
  , agent
  , complex
  , rule
  , site, site', sites
  , state, state'
  , fillSites
  , enumState, enumState', enumStates
  , declare
  , Statement(..)
  , AgentD(..)
  , VarD(..)
  , TokD(..)
  , Rule(..)
  , AgentP(..)
  , SiteP
  , LinkP(..)
  , StateP(..)
  , TokE(..)
  , Expr(..)
  ) where

import Prelude hiding (lookup)
import Flow.Internal.KappaParser(kappaParser)
import Flow.Internal.KappaQuotes(agent, complex, rule)
import Flow.Internal.KappaAST
import qualified Data.HashMap.Lazy as H
import qualified Data.List as L
import Data.Text(Text, pack)
\end{code}
}

\begin{code}
site :: Text -> AgentP -> Maybe (LinkP, StateP)
site x (AgentP _ sp) = H.lookup x sp

site' :: String -> AgentP -> Maybe (LinkP, StateP)
site' x = site (pack x)

sites :: AgentP -> [Text]
sites (AgentP _ sp) = H.keys sp

state :: Text -> AgentP -> Maybe StateP
state x a = case site x a of
  Nothing -> Nothing
  Just (_, s) -> Just s

state' :: String -> AgentP -> Maybe StateP
state' x = state (pack x)

-- | Derive agent declarations from a list of agent patterns
-- | as would be found in rules.
declare :: [AgentP] -> [AgentD]
declare agents =
  -- create a list (name, site, state) observed from the list of agents
  let aa = [ (name, s, state s a) | a@(AgentP name _) <- agents, s <- sites a ]
  -- separate into a list of lists by agent, and make a declaration for each
  in map mkagent (L.groupBy groupA $ L.sort aa)
  where
    -- predicate for grouping by agent (used above)
    groupA (n1, _, _) (n2, _, _) = n1 == n2
    -- predicate for grouping by site (used below)
    groupS (_, s1, _) (_, s2, _) = s1 == s2
    -- make an agent declaration
    mkagent :: [(Text, Text, Maybe StateP)] -> AgentD
    mkagent a =
      let (name, _, _):_ = a in
      AgentD name $ H.fromList $ map mksite $ L.groupBy groupS a
    -- make a site declaration
    mksite :: [(Text, Text, Maybe StateP)] -> (Text, [Text])
    mksite ss =
      let
        (_, sn, _):_ = ss
        states = [ st | (_, _, sx@(Just (State st))) <- ss, justState sx ]
      in
       -- an internal state that only appears once isn't worth the bother
       (sn, if length states > 1 then states else [])
    -- predicate for filtering out explicit states
    justState (Just _) = True
    justState _        = False

sitesD :: AgentD -> [Text]
sitesD (AgentD _ sp) = H.keys sp

statesD :: Text -> AgentD -> [Text]
statesD x (AgentD _ s) =
  case H.lookup x s of
   Just st -> st
   Nothing -> []

fillSites :: AgentD -> AgentP -> AgentP
fillSites (AgentD _ sd) (AgentP name sp) = AgentP name allsites
  where
    defaults = H.map (\_ -> (MaybeBound, Undefined)) sd
    allsites = H.unionWith (\l _ -> l) sp defaults

enumState :: Text -> AgentD -> AgentP -> [AgentP]
enumState x dec a@(AgentP name sp) =
  case state x a of
   Just Undefined -> genStates
   Just _         -> [a] -- state already defined
   Nothing        -> genStates
  where
    genStates =
      case statesD x dec of
       [] -> [a]
       is -> [AgentP name (H.insertWith link x (MaybeBound, State s) sp) |
              s <- is]
    link (_, s) (l, _) = (l, s)

enumState' :: String -> AgentD -> AgentP -> [AgentP]
enumState' x = enumState (pack x)

enumStates :: AgentD -> AgentP -> [AgentP]
enumStates dec a = enumStates' [a] (sitesD dec)
  where
    enumStates' aa [] = aa
    enumStates' aa (x:rest) =
      enumStates' (concat (map (enumState x dec) aa)) rest
\end{code}

% Local Variables:
% compile-command: "cd ..; cabal build && cabal test"
% End:
