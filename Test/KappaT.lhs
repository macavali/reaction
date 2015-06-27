\hide{
\begin{code}
{-# LANGUAGE QuasiQuotes #-}
module Test.KappaT (kappaTests) where

import Flow.Kappa
import Data.Attoparsec.Text(parseOnly)
import Data.HashMap.Lazy(fromList)
import Data.Text(pack)
import Test.HUnit hiding (State)
\end{code}
}
\begin{code}
kappaTests :: Test
kappaTests = "Kappa" ~: TestList [
  "Parser" ~: TestList testParseKappa
  , "EnumState" ~: TestList testEnumState
  ]

testParseKappa :: [Test]
testParseKappa = map (\(s,l) -> l ~=? parse s) $ k
  where k =
          [ ("%agent: A(z,y~u~p,x~0~1~2)",
             [AgentD "A" (fromList [ ("x", ["0", "1", "2"])
                                   , ("y", ["u", "p"])
                                   , ("z", [])
                                   ])
             ])
          , ("%token: atp\n%token: adp",
             [TokD "atp", TokD "adp"])
          , ("%var: 'V' 1\n%var: V 2e3",
             [VarD "V" (Lit 1.0), VarD "V" (Lit 2000.0)])
          , ("%var: v [exp](-u)",
             [VarD "v" (Exp (Neg (Var "u")))])
          , ("%var: v [max]([cos](u), [abs](-w))",
             [VarD "v" (Max (Cos (Var "u")) (Abs (Neg (Var "w"))))])
          , ("'my rule' A(x!2), B(y) -> B(y~0) @1",
             [RuleD "my rule" Rule {
                 lhs = ([ AgentP "A" (fromList [("x", ((Linked "2"), Undefined))])
                        , AgentP "B" (fromList [("y", (Unbound, Undefined))])
                        ], []),
                 rhs = ([AgentP "B" (fromList [("y", (Unbound, (State "0")))])
                        ], []),
                 rate = Lit 1.0
             } ])
          , ("'bi rule' A(x!_) <-> B(y) @k1, 'k2'",
             [ RuleD "bi rule" Rule {
                  lhs = ([ AgentP "A" (fromList [("x", (Bound, Undefined))])], []),
                  rhs = ([ AgentP "B" (fromList [("y", (Unbound, Undefined))])], []),
                  rate = Var "k1"
                  }
             , RuleD "bi rule" Rule {
                  lhs = ([ AgentP "B" (fromList [("y", (Unbound, Undefined))])], []),
                  rhs = ([ AgentP "A" (fromList [("x", (Bound, Undefined))])], []),
                  rate = Var "k2"
                  }
             ])
          , ("'hybrid rule' S(x!1~u),K(y!1) -> S(x~p),K(y) @k",
             [ RuleD "hybrid rule" Rule {
                  lhs = ([ AgentP "S" (fromList [("x", ((Linked "1"), (State "u")))])
                         , AgentP "K" (fromList [("y", ((Linked "1"), Undefined))])], []),
                  rhs = ([ AgentP "S" (fromList [("x", (Unbound, (State "p")))])
                         , AgentP "K" (fromList [("y", (Unbound, Undefined))])], []),
                  rate = Var "k"
                  }
             ])
          , ("'hybrid rule' S(x~u!1),K(y!1) | 0.1:atp -> S(x~p),K(y) | 0.1:adp @ 'k'",
             [ RuleD "hybrid rule" Rule {
                  lhs = ([ AgentP "S" (fromList [("x", ((Linked "1"), (State "u")))])
                         , AgentP "K" (fromList [("y", ((Linked "1"), Undefined))])
                         ], [Tok "atp" (Lit 0.1)]),
                  rhs = ([ AgentP "S" (fromList [("x", (Unbound, (State "p")))])
                         , AgentP "K" (fromList [("y", (Unbound, Undefined))])
                         ], [Tok "adp" (Lit 0.1)]),
                  rate = Var "k"
                  }
             ])
          ]
        parse s = case parseOnly kappaParser (pack s) of
          Right r  -> r
          Left err -> error $ err

testEnumState :: [Test]
testEnumState = [l ~=? s | (l,s) <- k]
  where k = [
          (declare [complex| A(x!1), A(x!1~2,y~p), A(x~1), B(u,v,w~1) |],
           [[agent| A(x~1~2,y) |], [agent| B(w,u,v) |]])
          ]

\end{code}
%% Local Variables:
%% compile-command: "cd ..; cabal build && cabal test"
%% End:
