#!/bin/sh
cabal build && ./dist-newstyle/build/x86_64-linux/ghc-9.2.8/yabg-0.1.0.0/x/yabg/build/yabg/yabg --rootpath "/" && rsync -r bin/* ~/src/sixkey.github.io
