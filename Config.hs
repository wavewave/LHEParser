{-# LANGUAGE ScopedTypeVariables #-}

module Config where
 
import Distribution.Simple
import Distribution.Simple.BuildPaths

import Distribution.Simple.Setup
import Distribution.PackageDescription
import Distribution.Simple.LocalBuildInfo

import System.Exit
import System.Process

import System.FilePath
import System.Directory


fortranCompileHook :: UserHooks
fortranCompileHook = simpleUserHooks { 
  -- preConf = compileFortran, -- \_ _ -> system "echo 'hi there'" >> return emptyHookedBuildInfo
  confHook = hookfunction 
--   postConf = hookfunction2, 
--  preBuild = hookfunction3
} 

{-
hookfunction3 args bf = do 
  hbi <- preBuild simpleUserHooks args bf 
  
  system "ls dist/build/autogen"

  return hbi
  -}

{-compileFortran x y = do 
   binfo <- preConffHook simpleUserHooks x y 
  r_pbi <- config binfo

  system "echo 'hi there'"
  return emptyHookedBuildInfo -} 


mkFortran :: FilePath -> FilePath -> IO () 
mkFortran dir file = do 
  let fn = dropExtension (takeFileName file)
  putStrLn $ "filename : " ++ fn 

  let objfile = dir </> fn ++ ".o"
  let command = "gfortran -m64 -o " ++ objfile ++ " -c " ++ file
  putStrLn $ "run fortran: " ++  command  
  system command 
 
  let libfile = "lib" ++ fn ++ ".a"

  let command2 = "ar cr " ++ (dir </> libfile) ++ " " ++ objfile

  putStrLn $ "run ar: " ++ command2 

  system command2  
  return () 

{-
hookfunction2 args cf pkg_descr lbi = do 
  postConf simpleUserHooks args cf pkg_descr lbi
  
--  let mdir = autogenModulesDir lbi
  let fortranfiles = filter (\x->takeExtension x == ".f") (extraSrcFiles pkg_descr)
--  putStrLn $ show mdir
  tdir <- getTemporaryDirectory
  putStrLn $ show fortranfiles  
  mapM_ (mkFortran tdir) fortranfiles -}
  

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
                                         ++ ["formatter"]
                                               -- ++ libs liboptset
                           , extraLibDirs = [tdir] -- libdirs liboptset 
                           , includeDirs = {- incdir : -} includeDirs buildinfo
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



{-
-- import Text.Parsec
-- import Control.Monad.Identity

config :: LocalBuildInfo -> IO (Maybe HookedBuildInfo)
config bInfo = do 
  (excode, out, err) <- readProcessWithExitCode "root-config" ["--glibs"] ""
  liboptset' <- case excode of 
                  ExitSuccess -> do  
--                    putStrLn $ show $ words out
--                    putStrLn $ show $ libraryOptions (words out)
--                    putStrLn $ show $ mkLibraryOptionSet . words $ out
--                    putStrLn $ show $ mkLibraryOptionSet . words $ out
                    return . Just .  mkLibraryOptionSet . words $ out
                  _ -> do 
                    putStrLn $ "root-config failure but I am installing HROOT without ROOT. It will not work. This is only for documentation." 
                    return Nothing              
  (excode2,out2,err2) <- readProcessWithExitCode "root-config" ["--incdir"] ""
  incdir' <- case excode2 of 
               ExitSuccess -> do  
--                 putStrLn $ out2
                 return . Just . head . words $ out2
               _ -> do 
                 putStrLn $ "root-config failure but I am installing HROOT without ROOT. It will not work. This is only for documentation." 
                 return Nothing
  let Just lib = library . localPkgDescr $ bInfo
      buildinfo = libBuildInfo lib
  let (r :: Maybe HookedBuildInfo) = case liboptset' of 
            Nothing -> Nothing
            Just liboptset -> 
              case incdir' of 
                Nothing -> Nothing 
                Just incdir -> 
                  let hbi = emptyBuildInfo { extraLibs = extraLibs buildinfo 
                                                         ++ libs liboptset
                                           , extraLibDirs = libdirs liboptset 
                                           , includeDirs = incdir : includeDirs buildinfo
                                           }
                  in Just (Just hbi, []) 
--  putStrLn $ "show here"
--  putStrLn $ show r
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

{-
libraryOptionClassifier :: ParsecT String () Identity LibraryOption
libraryOptionClassifier = 
  try (string "-L" >> many1 anyChar >>= return . Dir) 
  <|> try (string "-l" >> many1 anyChar >>= return . Lib)
  <|> (many1 anyChar >>= return . Opt)
-}

-}