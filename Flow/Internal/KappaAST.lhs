\hide{
\begin{code}
{-
    Reaction Networks -- Kappa Parser
    Copyright (C) 2015 William Waites

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
{-# LANGUAGE TemplateHaskell #-}
module Flow.Internal.KappaAST where
import Language.Haskell.TH.Syntax
import Data.List(intersperse)
import Data.Text(Text, pack, unpack)
import Data.HashMap.Lazy(HashMap, toList)
\end{code}
}

\begin{code}
data LinkP   = Bound | Unbound | MaybeBound | Linked Text
             deriving (Eq, Ord)
data StateP  = State Text | Undefined
             deriving (Eq, Ord)
data AgentP  = AgentP Text (HashMap Text (LinkP, StateP))
             deriving (Eq)
data TokE    = Tok Text Expr
             deriving (Show, Eq, Ord)
data Rule    = Rule { lhs  :: ([AgentP], [TokE])
                    , rhs  :: ([AgentP], [TokE])
                    , rate :: Expr
                    , desc :: Text
                    }
             deriving (Show, Eq)

defaultRule :: Rule
defaultRule = Rule { lhs = undefined
                   , rhs = undefined
                   , rate = Lit 1.0
                   , desc = pack "" }

data Expr =
  Var Text |
  Lit Double |
  Neg Expr |
  Abs Expr |
  Floor Expr |
  Exp Expr |
  Cos Expr |
  Sin Expr |
  Tan Expr |
  Log Expr |
  Min Expr Expr |
  Max Expr Expr |
  Plus Expr Expr |
  Times Expr Expr |
  Pow Expr Expr |
  Div Expr Expr |
  Mod Expr Expr
  deriving (Show, Eq, Ord)
           
data Statement =
  AgentD Text (HashMap Text [Text]) |
  VarD Text Expr |
  TokD Text |
  RuleD Rule
  deriving Eq

instance Show Statement where
  showsPrec _ (AgentD name sites) =
    showString (unpack name) . showString "(" . sitesp . showString ")"
    where
      sitesp = foldl1 (.) $
               intersperse (showString ",") (map (showSite) (toList sites))
      showSite (sn, []) = showString (unpack sn)
      showSite (sn, ss) =
        showString (unpack sn) . showString "~" .
        (foldl1 (.) $ itilde (map (showString . unpack ) ss))
      itilde ss = intersperse (showString "~") ss
  showsPrec n (VarD name expr) =
    showString "VarD" . showString (unpack name) . showsPrec n expr
  showsPrec _ (TokD name) =
    showString "TokD" . showString (unpack name)
  showsPrec n (RuleD rule) =
    showString "RuleD " . showString " " . showsPrec n rule
                               
instance Show AgentP where
  showsPrec n (AgentP name states) =
    showString (unpack name) . showString "(" . statesp . showString ")"
    where
      statesp = foldl1 (.) $ icomma (map showState (toList states))
      showState (sn, (l, s)) =
        showString (unpack sn) . showsPrec n l . showsPrec n s
      icomma ss = intersperse (showString ",") ss
instance Show LinkP where
  showsPrec _ Bound = showString "!_"
  showsPrec _ MaybeBound = showString "?"
  showsPrec _ Unbound = \s -> s
  showsPrec _ (Linked l) = showString "!" . showString (unpack l)

instance Show StateP where
  showsPrec _ Undefined  = \s -> s
  showsPrec _ (State s) = showString "~" . showString (unpack s)

packN :: Exp
packN = VarE $ mkName "Data.Text.pack"

unpackN :: Exp
unpackN = VarE $ mkName "Data.Text.unpack"

fromListN :: Exp
fromListN = VarE $ mkName "Data.HashMap.Lazy.fromList"

liftText :: Text -> Exp
liftText t = (AppE packN (LitE (StringL (unpack t))))

sitePN :: Exp
sitePN = ConE $ mkName "Flow.Kappa.SiteP"

agentPN :: Exp
agentPN = ConE $ mkName "Flow.Kappa.AgentP"

agentDN :: Exp
agentDN = ConE $ mkName "Flow.Kappa.AgentD"

ruleDN :: Exp
ruleDN = ConE $ mkName "Flow.Kappa.RuleD"

linkedN :: Exp
linkedN = ConE $ mkName "Flow.Kappa.Linked"

stateN :: Exp
stateN = ConE $ mkName "Flow.Kappa.State"

varN :: Exp
varN = ConE $ mkName "Flow.Kappa.Var"

litN :: Exp
litN = ConE $ mkName "Flow.Kappa.Lit"

tokN :: Exp
tokN = ConE $ mkName "Flow.Kappa.Tok"

siteN :: Exp
siteN = ConE $ mkName "Flow.Kappa.Site"

ruleN :: Name
ruleN = mkName "Flow.Kappa.Rule"

lhsN :: Name
lhsN  = mkName "Flow.Kappa.lhs"

rhsN :: Name
rhsN  = mkName "Flow.Kappa.rhs"

rateN :: Name
rateN = mkName "Flow.Kappa.rate"

descN :: Name
descN = mkName "Flow.Kappa.desc"

instance Lift AgentP where
  lift (AgentP name states) = do
    s <- mapM liftState (toList states)
    return (AppE
            (AppE agentPN (liftText name))
            (AppE fromListN (ListE s))
           )
    where
      liftState (k, (l, s)) = do
        ll <- lift l
        ss <- lift s
        return (TupE [liftText k, (TupE [ll, ss])])
        
instance Lift LinkP where
  lift Bound      = [| Bound |]
  lift Unbound    = [| Unbound |]
  lift MaybeBound = [| MaybeBound |]
  lift (Linked s) = return (AppE linkedN (liftText s))

instance Lift StateP where
  lift (State s) = return (AppE stateN (liftText s))
  lift Undefined  = [| Undefined |]

instance Lift Statement where
  lift (AgentD name sites)  = do
    return (AppE
            (AppE agentDN (liftText name))
            (AppE fromListN (ListE (map liftSite (toList sites)))))
    where
      liftSite (k, ss) =
        (TupE [liftText k, ListE (map liftText ss)])
  lift (RuleD rule) = do
    s <- lift rule
    return (AppE ruleDN s)
  lift _ = undefined

instance Lift Rule where
  lift (Rule { lhs=lh, rhs=rh, rate=r, desc=d }) = do
    llh <- lift lh
    lrh <- lift rh
    lr  <- lift r
    return (RecConE ruleN [ (lhsN, llh)
                          , (rhsN, lrh)
                          , (rateN, lr)
                          , (descN, liftText d) ])

instance Lift TokE where
  lift (Tok name expr) = do
    e <- lift expr
    return (AppE (AppE tokN (liftText name)) e)


instance Lift Expr where
  lift (Var name) = return e
    where e = (AppE varN (liftText name))
  lift (Lit d) = return e
    where e = (AppE litN (LitE (RationalL (toRational d))))
  lift (Neg e) = [| Neg e |]
  lift (Abs e) = [| Abs e |]
  lift (Floor e) = [| Floor e |]
  lift (Exp e) = [| Exp e |]
  lift (Cos e) = [| Cos e |]
  lift (Sin e) = [| Sin e |]
  lift (Tan e) = [| Tan e |]
  lift (Log e) = [| Log e |]
  lift (Min e1 e2) = [| Min e1 e2 |]
  lift (Max e1 e2) = [| Max e1 e2 |]
  lift (Plus e1 e2) = [| Plus e1 e2 |]
  lift (Times e1 e2) = [| Times e1 e2 |]
  lift (Pow e1 e2) = [| Pow e1 e2 |]
  lift (Div e1 e2) = [| Div e1 e2 |]
  lift (Mod e1 e2) = [| Mod e1 e2 |]

\end{code}

% Local Variables:
% compile-command: "cd ../..; cabal build && cabal test"
% End:
