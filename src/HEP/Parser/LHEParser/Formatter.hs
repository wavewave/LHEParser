-- | Formatting back to LHE format. Currently this uses Text.Printf and String, so it's slow.
--   Later, when text-format library becomes mature, I will switch to Data.Text.
--


module HEP.Parser.LHEParser.Formatter ( 
  printPtlInfo
) where

import Text.Printf

import HEP.Parser.LHEParser.Type

import Control.Applicative 

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