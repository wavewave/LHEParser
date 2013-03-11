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

--   let pinfo = PtlInfo 1 8 32 (3,0) (4,4) (3.2,233.3,1010.0,0.332,0.0003) 2.0 1.0

-- act = do 
{-    runMaybeT $ do 
      ev <- MaybeT await 
      liftIO $ print ev
    act-} 


main :: IO ()
main = do 
  args <- getArgs 
  let fp = args !! 0 
  h <- openFile fp ReadMode
  sourceHandle h =$= ungzip =$= parseBytes def =$= parseEvent $$ 
    awaitForever $ \(LHEvent einfo pinfos) -> do 
     forM_ pinfos $ \pinfo -> do 
       if check pinfo 
         then return () -- do 
           -- liftIO $ putStrLn "-----------------"
           -- liftIO $ print (formatParticleInfo pinfo)
           -- liftIO $ print (formatParticleInfoOld pinfo)
         else do 
           liftIO $ putStrLn "WRONGWRONGWRONGWRONG"
           liftIO $ print (formatParticleInfo pinfo)
           liftIO $ print (formatParticleInfoOld pinfo)
  
main' = do
  putStrLn (fformats [P (F 8 3) 9.000000000]) 

-- 9999999999])

{-  let lst1 = unsafePerformIO $ newArray (map fromIntegral [3,0])
      lst2 = unsafePerformIO $ newArray (map fromIntegral [4,4])
      lst3 = unsafePerformIO $ newArray (map realToFrac [3.2,233.3,1010.0,0.332,0.0003])

  let cstr2 = c_formatParticleInfo (fromIntegral (8::Int)) 
                                   (fromIntegral (32::Int))
                                   lst1
                                   lst2
                                   lst3 
                                   (realToFrac (2.0::Double))
                                   (realToFrac (1.0::Double))
      str2 = unsafePerformIO $ peekCString cstr2
  putStrLn $ str2 -}

  