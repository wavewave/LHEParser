{-# LANGUAGE ForeignFunctionInterface #-}

module Main where 

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Maybe 
import Data.Conduit 
import Data.Conduit.Binary
import qualified Data.Conduit.List as CL
import Data.Conduit.Zlib
import System.Environment
import System.IO
import Text.XML.Stream.Parse
-- 
import HEP.Util.Formatter.Fortran
-- 
import HEP.Parser.LHE.Conduit 
import HEP.Parser.LHE.Type
import HEP.Parser.LHE.Formatter

check :: PtlInfo -> Bool
check pinfo =
  let s1 = formatParticleInfo pinfo 
      s2 = formatParticleInfoOld pinfo 
  in (s1 == s2) 


main :: IO ()
main = do 
  args <- getArgs 
  let fp = args !! 0 
  h <- openFile fp ReadMode
  sourceHandle h =$= ungzip =$= parseBytes def =$= parseEvent $$ 
    awaitForever $ \(LHEvent einfo pinfos) -> do 
     forM_ pinfos $ \pinfo -> do 
       if check pinfo 
         then return () 
         else do 
           liftIO $ putStrLn "Mismatch happened"
           liftIO $ print (formatParticleInfo pinfo)
           liftIO $ print (formatParticleInfoOld pinfo)
  
main' = do
  putStrLn (fformats [ P (E 11 3) 9.99999999
                     , P (E 11 3) 8.99999999
                     , P (E 17 9) 3.32239
                     , P (F 8 3) 3.2999 
                     , P (F 8 3) 9.9999 ])

  