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
 * Investigate things in the REPL. For example,
```
% cabal repl
Flow.Kappa> declare [complex| A(x!1), A(x~2!1,y~p), A(x~1), B(u,v,w~1) |]
[A(x~1~2,y),B(w,u,v)]
```
