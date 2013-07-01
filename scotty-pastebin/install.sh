#!/bin/sh
cd ../eval-api/ &&
cabal build && 
cabal install &&
cp dist/build/evali/evali /idia/run/bin/evali &&
sudo restorecon /idia/run/bin/evali &&
cd ../scotty-pastebin &&
cabal configure &&
cabal build &&
cp dist/build/scotty-pastebin/scotty-pastebin /idia/run/bin/scotty-pastebin &&
sudo restorecon /idia/run/bin/scotty-pastebin
