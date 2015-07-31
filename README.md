## Kappa and Reaction Networks

### Quick start:

 * [optional] Make sure you have a recent version of the cabal tool
   that supports "sandbox" and do
```
% cabal sandbox init
```
 * Install dependencies
```
% cabal install --only-dependencies
```
 * Check that everything is working properly
```
% cabal test
...
1 of 1 test suites (1 of 1 test cases) passed.
```
 * Investigate things in the REPL. For example,
```
% cabal repl
Flow.Kappa> declare [complex| A(x!1), A(x~2!1,y~p), A(x~1), B(u,v,w~1) |]
[A(x~1~2,y),B(w,u,v)]
```
 * Build the tools
```
% cabal build
```
 * Look at a materialised RDF version of an example model
```
% ./dist/build/krdf/krdf -f examples/tcs.kappa
```
