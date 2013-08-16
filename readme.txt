How to build Claq

Claq uses the Hacq library.  We assume hacq.cabal is in <dir>/hacq and claq.cabal is in <dir>/claq.

1. Build and install Hacq by using cabal-dev as stated in readme.txt in Hacq.  You do not have to build executables bundled with Hacq to use Claq.  That is, you have to follow steps 1-4 in readme.txt in Hacq, but you do not have to follow step 5.

2. In <dir>/claq, run:

cabal-dev install free --sandbox=../hacq/cabal-dev
cabal-dev configure --sandbox=../hacq/cabal-dev
cabal-dev build --sandbox=../hacq/cabal-dev

This creates an executable file claq (or claq.exe on Windows) in directory <dir>/claq/dist/build/claq.
