#! /usr/bin/env runhaskell

> import Distribution.Simple
> import Distribution.PackageDescription
> 
> import Config
> 
> main = defaultMainWithHooks fortranCompileHook
> 
> -- fortranCompileHook = simpleUserHooks { 
> --  preConf = \_ _ -> system "echo 'hi there'" >> return emptyHookedBuildInfo
> -- } 
