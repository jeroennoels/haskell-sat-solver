#!/bin/bash
find ../haskell-sat-solver/ -name "*.o"  -print0 | xargs -r -0 /bin/rm
find ../haskell-sat-solver/ -name "*.hi" -print0 | xargs -r -0 /bin/rm
find ../haskell-sat-solver/ -name "*~"   -print0 | xargs -r -0 /bin/rm
/bin/rm -f ../haskell-sat-solver/test/Main
