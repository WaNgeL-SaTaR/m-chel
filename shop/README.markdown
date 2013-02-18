# INSTALL
  
  $ sudo apt-get install couchdb haskell-platform happstack
  $ cabal update
  $ cabal install couchdb-conduit aeson uuid

Set auth information in:
  $ nano Config.hs

Compile and run:
  $ ghc shop.hs && ./shop

Then open in browser:
<http://localhost:7003/>
