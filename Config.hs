{-# LANGUAGE ScopedTypeVariables #-}

module Config where


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
  -- preConf = compileFortran, -- \_ _ -> system "echo 'hi there'" >> return emptyHookedBuildInfo

    confHook = hookfunction
  , instHook = hook4InstHook
--  , buildHook = hook4Build
--   postConf = hookfunction2, 
--  preBuild = hookfunction3
   

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


{-  let libPref = libdir . absoluteInstallDirs pkg_descr lbi . fromFlagOrDefault $ NoFlag
             -- . fromFlag . (defaultCopyFlags { copyDest = toFlag NoCopyDest } ) -- installDistPref  
             -- $ flags -}

  

  tdir <- getTemporaryDirectory

  --  let dylibfile =  dir </> "lib" ++ fn ++ ".dylib"


  let command = "mv " ++ (tdir </> "libfformatter.a") ++" " ++ (tdir </> "libfformatter.dylib") ++ " " ++  libPref 
  putStrLn command
  system command
  return () 
 
 



{-
flibPaths :: IO FilePath 
flibPaths = do 
  let fn = dropExtension (takeFileName file)
  let libfile = "lib" ++ fn ++ ".a"
  -}

mkFortran :: FilePath -> FilePath -> IO () 
mkFortran dir file = do 
  let fn = dropExtension (takeFileName file)
  putStrLn $ "filename : " ++ fn 

  let objfile = dir </> fn ++ ".o"
  let command = "gfortran -m64 -o " ++ objfile ++ " -c " ++ file
  putStrLn $ "run fortran: " ++  command  
  system command 
 
  let libfilename = "libfformatter.a"
      dylibfilename = "libfformatter.dylib"
      sofilename = "libfformatter.so"

  let command2 = "ar cr " ++ (dir </> libfilename) ++ " " ++ objfile

  putStrLn $ "run ar: " ++ command2 

  system command2  

  putStrLn $ "make shared object"
  putStrLn $ os   

  let command3 = case buildOS of 
        OSX -> "gcc -dynamiclib -Wl,-headerpad_max_install_names,-undefined,dynamic_lookup,-compatibility_version,1.0,-current_version,1.0 -o " ++ (dir </> dylibfilename) ++ " " ++ objfile 
        Linux -> "gcc -shared -Wl,-soname,libfformatter.so -o " ++ (dir </> sofilename) ++ " " ++ objfile
        _ -> error "cannot handle this OS" 

  system command3

  return () 


hook4Build  pkg_descr lbi uhook bf = do 
  let Just lib = library pkg_descr
      libBi = libBuildInfo lib
      cSrcs = cSources libBi
      new_cSrcs = cSrcs ++ ["fformatter.c"]
      new_libBi = libBi { cSources = new_cSrcs } 
      new_lib = lib { libBuildInfo = new_libBi } 
      new_pkg_descr = pkg_descr { library = Just new_lib } 


  buildHook simpleUserHooks new_pkg_descr lbi uhook bf

  
--  let mdir = autogenModulesDir lbi
{-  let fortranfiles = filter (\x->takeExtension x == ".f") (extraSrcFiles pkg_descr)
--  putStrLn $ show mdir
  tdir <- getTemporaryDirectory
  putStrLn $ show fortranfiles  
  mapM_ (mkFortran tdir) fortranfiles 
 -} 

hookfunction x y = do 
  binfo <- confHook simpleUserHooks x y 
  let pkg_descr = localPkgDescr binfo
  tdir <- getTemporaryDirectory
  -- autogenModulesDir binfo
  let fortranfiles = filter (\x->takeExtension x == ".f") (extraSrcFiles pkg_descr)
  putStrLn $ show tdir
  putStrLn $ show fortranfiles  

  mapM_ (mkFortran tdir) fortranfiles 

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
                           , extraLibDirs = [tdir] -- libdirs liboptset 
                           -- , includeDirs = {- incdir : -} includeDirs buildinfo
                           }
  let (r :: Maybe HookedBuildInfo) = Just (Just hbi, []) 
  putStrLn $ show hbi
  return r 


data LibraryOptionSet = LibraryOptionSet { 
  libs :: [String], 
  libdirs :: [String], 
  libopts :: [String]
} deriving Show

data LibraryOption = Lib String 
                   | Dir String
                   | Opt String 
                   deriving Show

mkLibraryOptionSet :: [String] -> LibraryOptionSet
mkLibraryOptionSet strs = let opts = libraryOptions strs
                          in  foldr f (LibraryOptionSet [] [] []) opts 
  where f x (LibraryOptionSet l d o) = case x of
                                         Lib st -> LibraryOptionSet (st:l) d o 
                                         Dir st -> LibraryOptionSet l (st:d) o 
                                         Opt st -> LibraryOptionSet l d (st:o) 

libraryOptions :: [String] -> [LibraryOption] -- LibraryOptionSet 
libraryOptions = map f 
  where f x = let r = parseLibraryOptionClassifier x
              in  case r of 
                    Left msg -> error (show msg)
                    Right result -> result


parseLibraryOptionClassifier :: String -> Either String LibraryOption 
parseLibraryOptionClassifier [] = Left "empty option"
parseLibraryOptionClassifier str@(x:xs) = 
  case x of
    '-' -> if null xs 
             then Left "parse error"
             else let (y:ys) = xs
                  in  case y of
                        'L' -> Right (Dir ys)
                        'l' -> Right (Lib ys)
                        _ -> Right (Opt str)
    _ -> Right (Opt str) 




