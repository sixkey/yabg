#!/bin/sh

inotifywait -m -r xlogin src/Main.hs -e create -e moved_to -e modify |
    while read -r directory action file; do
        cabal build && ./dist-newstyle/build/x86_64-linux/ghc-9.2.8/yabg-0.1.0.0/x/yabg/build/yabg/yabg --rootpath "/" -d
    done
