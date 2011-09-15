{-# LANGUAGE ScopedTypeVariables #-}

module Config where

import Data.List 

import Distribution.Simple
import Distribution.Simple.BuildPaths

import Distribution.Simple.Setup
import Distribution.PackageDescription
import Distribution.Simple.LocalBuildInfo

import Distribution.System

import System.Exit
import System.Process

import System.FilePath
import System.Directory

import System.Info




fortranCompileHook :: UserHooks
fortranCompileHook = simpleUserHooks { 
  confHook = hookfunction, 
  instHook = hook4InstHook
} 

hook4InstHook :: PackageDescription -> LocalBuildInfo -> UserHooks -> InstallFlags -> IO () 
hook4InstHook pkg_descr lbi uhook flags = do 
  instHook simpleUserHooks pkg_descr lbi uhook flags
  let copyFlags = defaultCopyFlags { 
                    copyDistPref = installDistPref flags
                  , copyDest     = toFlag NoCopyDest 
                  , copyVerbosity = installVerbosity flags
                  }
      copydest = fromFlag (copyDest (copyFlags))
      libPref = libdir . absoluteInstallDirs pkg_descr lbi $ copydest
  tdir <- getTemporaryDirectory
  let fortranfiles = filter (\x->takeExtension x == ".f") (extraSrcFiles pkg_descr)
--   mkFortran tdir libPref fortranfiles


  let command = "mv " ++ (tdir </> "libfformatter.a") ++ " " ++ libPref 
  putStrLn command
  system command

{-
" " ++ (libPref  </> "libfformatter.dylib") ++ " " ++  libPref   
  let objfile = dir </> fn ++ ".o"
  return ()  -}

  let objfile file = 
        let fn = dropExtension (takeFileName file)
        in  tdir </> fn ++ ".o"

  let objfiles = map objfile fortranfiles 
      ofileargs = intercalate " " objfiles

  let libfilename = "libfformatter.a"
      dylibfilename = "libfformatter.dylib"
      sofilename = "libfformatter.so"

  putStrLn $ "make shared object"
  putStrLn $ os   
  let command3 = case buildOS of 
        OSX -> "gcc -dynamiclib -Wl,-headerpad_max_install_names,-undefined,dynamic_lookup,-compatibility_version,1.0,-current_version,1.0 -o " ++ (libPref </> dylibfilename) ++ " " ++ ofileargs
        Linux -> "gcc -shared -Wl,-soname,libfformatter.so -o " ++ (libPref </> sofilename) ++ " " ++ ofileargs
        _ -> error "cannot handle this OS" 
  putStrLn $ command3 
  system command3
  return () 



mkFortran :: FilePath -> [FilePath] -> IO () 
mkFortran tdir files = do 

  -- compilation

  let objfile file = 
        let fn = dropExtension (takeFileName file)
        in  tdir </> fn ++ ".o"
  let compile file = do
        let command = "gfortran -m64 -o " ++ objfile file ++ " -c " ++ file
        putStrLn $ "run fortran: " ++  command  
        system command 
  mapM_ compile files 

  -- linking 
  let libfilename = "libfformatter.a"
  let objfiles = map objfile files 
      ofileargs = intercalate " " objfiles
  let command2 = "ar cr " ++ (tdir </> libfilename) ++ " " ++ ofileargs
  putStrLn $ "run ar: " ++ command2 
  system command2  
  return ()

{-
hook4Build  pkg_descr lbi uhook bf = do 
  let Just lib = library pkg_descr
      libBi = libBuildInfo lib
      cSrcs = cSources libBi
      new_cSrcs = cSrcs ++ ["fformatter.c"]
      new_libBi = libBi { cSources = new_cSrcs } 
      new_lib = lib { libBuildInfo = new_libBi } 
      new_pkg_descr = pkg_descr { library = Just new_lib } 
  buildHook simpleUserHooks new_pkg_descr lbi uhook bf
-}


hookfunction x y = do 
  binfo <- confHook simpleUserHooks x y 
  let pkg_descr = localPkgDescr binfo
  tdir <- getTemporaryDirectory
  -- autogenModulesDir binfo
  let fortranfiles = filter (\x->takeExtension x == ".f") (extraSrcFiles pkg_descr)
  putStrLn $ show tdir
  putStrLn $ show fortranfiles  
  mkFortran  tdir fortranfiles 
  r_pbi <- config binfo
  let newbinfo = case r_pbi of 
                   Just pbi ->  binfo { localPkgDescr = updatePackageDescription pbi pkg_descr }
                   Nothing -> do 
                     let r_lib = library pkg_descr 
                     case r_lib of
                       Just lib ->  
                         let binfo2 = libBuildInfo lib
                             newlib = lib { libBuildInfo = binfo2 { cSources = [] }}  
                         in  binfo { localPkgDescr = pkg_descr { library = Just newlib }}  
                       Nothing -> error "some library setting is wrong." 
--   putStrLn (show (localPkgDescr newbinfo))
  return newbinfo

config :: LocalBuildInfo -> IO (Maybe HookedBuildInfo)
config bInfo = do 
  tdir <- getTemporaryDirectory
  let Just lib = library . localPkgDescr $ bInfo
      buildinfo = libBuildInfo lib
  let hbi = emptyBuildInfo { extraLibs = extraLibs buildinfo 
                                         ++ ["fformatter"]
                                               -- ++ libs liboptset
                           , extraLibDirs = [tdir]  -- libdirs liboptset 
                           -- , includeDirs = {- incdir : -} includeDirs buildinfo
                           }
  let (r :: Maybe HookedBuildInfo) = Just (Just hbi, []) 
  putStrLn $ show hbi
  return r 
