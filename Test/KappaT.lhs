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
  , "RuleQQ" ~: TestList testRuleQQ
  , "EnumState" ~: TestList testEnumState
  ]

testParseKappa :: [Test]
testParseKappa = map (\(s,l) -> l ~=? parse s) $ k
  where k =
          [ ("%agent: A(z,y~u~p,x~0~1~2)",
             [AD $ AgentD "A" (fromList [ ("x", ["0", "1", "2"])
                                   , ("y", ["u", "p"])
                                   , ("z", [])
                                   ])
             ])
          , ("%token: atp\n%token: adp",
             [TD $ TokD "atp", TD $ TokD "adp"])
          , ("%var: 'V' 1\n%var: V 2e3",
             [VD $ VarD "V" (Lit 1.0), VD $ VarD "V" (Lit 2000.0)])
          , ("%var: v [exp](-u)",
             [VD $ VarD "v" (Exp (Neg (Var "u")))])
          , ("%var: v [max]([cos](u), [abs](-w))",
             [VD $ VarD "v" (Max (Cos (Var "u")) (Abs (Neg (Var "w"))))])
          , ("'my rule' A(x!2), B(y) -> B(y~0) @1",
             [RD Rule {
                 lhs = ([ AgentP "A" (fromList [("x", ((Linked "2"), Undefined))])
                        , AgentP "B" (fromList [("y", (Unbound, Undefined))])
                        ], []),
                 rhs = ([AgentP "B" (fromList [("y", (Unbound, (State "0")))])
                        ], []),
                 rate = Lit 1.0,
                 desc = pack "my rule"
             } ])
          , ("'bi rule' A(x!_) <-> B(y) @k1, 'k2'",
             [ RD Rule {
                  lhs = ([ AgentP "A" (fromList [("x", (Bound, Undefined))])], []),
                  rhs = ([ AgentP "B" (fromList [("y", (Unbound, Undefined))])], []),
                  rate = Var "k1",
                  desc = pack "bi rule"
                  }
             , RD Rule {
                  lhs = ([ AgentP "B" (fromList [("y", (Unbound, Undefined))])], []),
                  rhs = ([ AgentP "A" (fromList [("x", (Bound, Undefined))])], []),
                  rate = Var "k2",
                  desc = pack "bi rule"
                  }
             ])
          , ("'hybrid rule' S(x!1~u),K(y!1) -> S(x~p),K(y) @k",
             [ RD Rule {
                  lhs = ([ AgentP "S" (fromList [("x", ((Linked "1"), (State "u")))])
                         , AgentP "K" (fromList [("y", ((Linked "1"), Undefined))])], []),
                  rhs = ([ AgentP "S" (fromList [("x", (Unbound, (State "p")))])
                         , AgentP "K" (fromList [("y", (Unbound, Undefined))])], []),
                  rate = Var "k",
                  desc = pack "hybrid rule"
                  }
             ])
          , ("'hybrid rule' S(x~u!1),K(y!1) | 0.1:atp -> S(x~p),K(y) | 0.1:adp @ 'k'",
             [ RD Rule {
                  lhs = ([ AgentP "S" (fromList [("x", ((Linked "1"), (State "u")))])
                         , AgentP "K" (fromList [("y", ((Linked "1"), Undefined))])
                         ], [Tok "atp" (Lit 0.1)]),
                  rhs = ([ AgentP "S" (fromList [("x", (Unbound, (State "p")))])
                         , AgentP "K" (fromList [("y", (Unbound, Undefined))])
                         ], [Tok "adp" (Lit 0.1)]),
                  rate = Var "k",
                  desc = pack "hybrid rule"
                  }
             ])
          ]
        parse s = case parseOnly kappaParser (pack s) of
          Right r  -> r
          Left err -> error $ err

testRuleQQ :: [Test]
testRuleQQ = [l ~=? s | (l,s) <- t]
  where t = [ ([rule| 'homodimer' A(x), A(x) -> A(x!1), A(x!1) @1.0 |],
               [ Rule {
                  lhs = ([ AgentP "A" (fromList [("x", (Unbound, Undefined))])
                         , AgentP "A" (fromList [("x", (Unbound, Undefined))])], []),
                  rhs = ([ AgentP "A" (fromList [("x", (Linked "1", Undefined))])
                         , AgentP "A" (fromList [("x", (Linked "1", Undefined))])], []),
                  rate = Lit 1.0,
                  desc = pack "homodimer"
                 } ])
            ]

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
