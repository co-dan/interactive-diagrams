#!/bin/sh
cd ../eval-api/ &&
cabal build && cabal install &&
cd ../scotty-pastebin &&
cabal configure &&
cabal build &&
cp dist/build/scotty-pastebin/scotty-pastebin /idia/run/bin/scotty-pastebin &&
sudo restorecon /idia/run/bin/scotty-pastebin
