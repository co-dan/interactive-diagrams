#!/bin/sh
killall -9 scotty-pastebin
killall -9 eval-service
# cabal install workers/ eval-api/ &&
cd ./eval-api/ &&
cabal build && 
 cabal install . ../workers &&
cp dist/build/evali/evali /idia/run/bin/evali &&
cp dist/build/eval-service/eval-service /idia/run/bin/eval-service &&
sudo restorecon /idia/run/bin/evali &&
sudo restorecon /idia/run/bin/eval-service && 
cd ../scotty-pastebin &&
cabal configure &&
cabal build &&
cp dist/build/scotty-pastebin/scotty-pastebin /idia/run/bin/scotty-pastebin &&
sudo restorecon /idia/run/bin/scotty-pastebin &&
cd .. &&
cp -R common/* /idia/run/common/
