#!/usr/bin/env runhaskell
{-
  In addition to a .cabal file, a package must contain a setup file.
  This allows Cabal's build process to be heavily customized
  (if a package needs it).

  Once we write the .cabal and Setup.hs files, there are three steps left:
  
  1. To instruct Cabal how to build and where to install a package, we run 
  a simple command:
  
  $ runghc Setup configure
  
  If we do not provide any arguments to configure, Cabal will install our
  package in the system-wide package database.
  To install it into our home directory and our personal package database, we 
  must provide a little more information:TODO
  
  2. We build the package:
  
  $runghc Setup build
  
  3. If this succeeds, we can install the package.
  
-}

import Distribution.Simple
main = defaultMain
