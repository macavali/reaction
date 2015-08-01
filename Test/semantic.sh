#!/bin/sh

ROOT=`pwd`
export PATH=${ROOT}/dist/build/krdf:$PATH

exit 0

# Local Variables:
# compile-command: "cd ..; cabal build; cabal test"
# End:
