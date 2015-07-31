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
% ./dist/build/krdf/krdf -f examples/tcs.kappa -a -n -m
```

### The krdf tool

The krdf tool transforms annotated Kappa files into RDF. By default
it will produce an empty graph as output. It has command line flags
to extract the annotations from the comments in the file (see the
`examples/tcs.kappa` file), to normalise agent patterns in rules
according to the declarations, and to materialise the rules into
RDF. All three of these options used together will extract the maximum
information from the source file. Using the normalise flag only makes
sense together with materialise since it operates on the information
latent in the Kappa rules.
```
Kappa -> RDF

Usage: krdf (-f|--filename FILENAME) [-a|--annotations] [-m|--materialise]
            [-n|--normalise]
  Transform Kappa rules to RDF

Available options:
  -h,--help                Show this help text
  -f,--filename FILENAME   Kappa file to read
  -a,--annotations         Extract annotations
  -m,--materialise         Materialise Kappa statements to RDF
  -n,--normalise           Normalise agent patterns according to declarations
```
