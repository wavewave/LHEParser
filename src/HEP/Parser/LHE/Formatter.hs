{-# LANGUAGE RecordWildCards #-}

-----------------------------------------------------------------------------
-- |
-- Module      : HEP.Parser.LHE.Formatter
-- Copyright   : (c) 2010-2013 Ian-Woo Kim
--
-- License     : GPL-3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--
-- Formatting back to LHE format. Currently this uses Text.Printf and String, so it's slow.
-- Later, when text-format library becomes mature, I will switch to Data.Text.
--
-----------------------------------------------------------------------------

module HEP.Parser.LHE.Formatter ( 
    printPtlInfo
  , formatLHEvent
  , formatEventInfo
  , formatEventInfoOld
  , formatParticleInfo
  , formatParticleInfoOld
  ) where

import Control.Applicative 
import Data.List
import Foreign.C
import Foreign.Marshal.Array 
import System.IO.Unsafe
import Text.Printf
--
import HEP.Util.Formatter.Fortran
-- 
import HEP.Parser.LHE.Type
import HEP.Parser.LHE.Formatter.Internal


px :: PtlInfo -> Double
px pinfo = case pup pinfo of 
             (x,_,_,_,_) -> x

py :: PtlInfo -> Double
py pinfo = case pup pinfo of 
             (_,y,_,_,_) -> y

pz :: PtlInfo -> Double
pz pinfo = case pup pinfo of 
             (_,_,z,_,_) -> z

p0 :: PtlInfo -> Double
p0 pinfo = case pup pinfo of
             (_,_,_,t,_) -> t

mass :: PtlInfo -> Double
mass pinfo = case pup pinfo of 
               (_,_,_,_,m) -> m

printPtlInfo :: PtlInfo -> String 
printPtlInfo pinfo = 
  printf "    %5d %5d %5d %5d %5d %5d %1.10E %18.11e %18.11E %18.11E %18.11E %f %f" 
  <$> idup <*> istup <*> fst.mothup <*> snd.mothup <*> fst.icolup <*> snd.icolup 
  <*> px <*> py <*> pz <*> p0 <*> mass <*> vtimup <*> spinup $ pinfo

formatLHEvent :: LHEvent -> String 
formatLHEvent (LHEvent einfo pinfos) = 
  let estr = formatEventInfo einfo
      pstrs = map formatParticleInfo pinfos
  in  intercalate "\n" (estr:pstrs)


formatEventInfo :: EventInfo -> String
formatEventInfo EvInfo {..} = 
  fformats [ P (I 2) nup 
           , P (I 4) idprup 
           , P (E 15 7) xwgtup
           , P (E 15 7) scalup
           , P (E 15 7) aqedup
           , P (E 15 7) aqcdup ]

-- | using old fortran 
formatParticleInfo :: PtlInfo -> String 
formatParticleInfo PtlInfo {..} = 
  let fmt2 f (a,b) = [P f a,P f b] 
      fmt5 f (a,b,c,d,e) = [P f a,P f b,P f c,P f d,P f e] 
  in fformats $    [ P (I 9) idup
                   , P (I 5) istup ] 
                ++ fmt2 (I 5) mothup  
                ++ fmt2 (I 5) icolup 
                ++ fmt5 (E 19 11) pup
                ++ [ P (F 3 0) vtimup
                   , P (F 4 0) spinup ]  
 


-- | using old fortran
formatEventInfoOld :: EventInfo -> String 
formatEventInfoOld einfo =
  let cstr = c_formatEventInfo <$> fromIntegral.nup
                               <*> fromIntegral.idprup
                               <*> realToFrac.xwgtup
                               <*> realToFrac.scalup
                               <*> realToFrac.aqedup
                               <*> realToFrac.aqcdup $ einfo
  in  unsafePerformIO $ peekCString cstr
  
-- | using old fortran 
formatParticleInfoOld :: PtlInfo -> String 
formatParticleInfoOld pinfo = 
  let tuple2lst (a,b) = [a,b]
      tuple5lst (a,b,c,d,e) = [a,b,c,d,e] 
      lstToCPtr f = unsafePerformIO . newArray . (map f) 
      cstr = c_formatParticleInfo 
             <$> fromIntegral.idup
             <*> fromIntegral.istup
             <*> lstToCPtr fromIntegral . tuple2lst . mothup
             <*> lstToCPtr fromIntegral . tuple2lst . icolup
             <*> lstToCPtr realToFrac . tuple5lst . pup
             <*> realToFrac . vtimup
             <*> realToFrac . spinup 
             $ pinfo
  in  unsafePerformIO $ peekCString cstr


{-
main :: IO ()
main = do 
  putStrLn "haha"
  putStrLn $ formatEventInfo $ EvInfo 2 2 4 4 4 4  
  putStrLn $ formatParticleInfo $ PtlInfo 1 8 32 (3,0) (4,4) (3.2,233.3,1010.0,0.332,0.0003) 2.0 1.0
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

  -}