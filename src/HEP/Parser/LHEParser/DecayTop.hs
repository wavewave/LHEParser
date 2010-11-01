module HEP.Parser.LHEParser.DecayTop where

import HEP.Parser.LHEParser.Type
import qualified Data.Map as M
import Data.List

findonlyTerminal :: DecayTop a -> [DecayTop a] -> [DecayTop a] 
findonlyTerminal (Terminal x) xs = (Terminal x) : xs 
findonlyTerminal (Decay (x,xs)) ys = foldr findonlyTerminal ys xs 


mkOrdDecayTop :: (Ord a) => DecayTop a -> DecayTop a 
mkOrdDecayTop (Decay (x, xs)) = let ordchild = map mkOrdDecayTop xs
                                in  Decay (x, sort xs) 
mkOrdDecayTop (Terminal x) = Terminal x


mkDecayPDGExtTop :: PtlInfoMap -> DecayTop PtlID -> DecayTop PtlIDInfo  
mkDecayPDGExtTop pmap idtop = fmap f idtop 
  where f pid = let pinfo = maybe undefined id (M.lookup pid pmap)
                in PIDInfo (idup pinfo) pinfo


mkDecayPDGTop :: PtlInfoMap -> DecayTop PtlID -> DecayTop PDGID
mkDecayPDGTop pmap idtop = fmap f idtop  
  where f pid = let pinfo = maybe undefined id (M.lookup pid pmap)
                in  idup pinfo
  

mkFullDecayTop :: IntTree -> [DecayTop PtlID] 
mkFullDecayTop tree = let lst = outgoing (cross tree)
                          dmap = decaymap tree
                      in map (mkDecayTop dmap) lst

mkDecayTop :: DecayMap -> PtlID -> DecayTop PtlID
mkDecayTop dmap pid = let dlist = (M.lookup pid dmap)
                      in  case dlist of 
                        Nothing -> Terminal pid 
                        Just ls -> Decay (pid, map (mkDecayTop dmap) ls)

mkIntTree :: [PtlInfo] -> IntTree
mkIntTree = foldr mkIntTreeWkr (IntTree (Cross [] []) M.empty)

mkIntTreeWkr :: PtlInfo -> IntTree -> IntTree
mkIntTreeWkr info tree@(IntTree cr dmap )  
  | newm1 == 0 && newm2 == 0 = IntTree (Cross (newptlid : inptl) outptl) dmap
  | newm1 /= newm2           = IntTree (Cross inptl (newptlid : outptl)) dmap  
  | newm1 == newm2           = let ndmap = M.insertWith updtr newm1 [newptlid] dmap 
                               in IntTree cr ndmap
  | otherwise   = undefined
  where inptl  = incoming cr
        outptl = outgoing cr
        newptlid = ptlid info  
        (newm1,newm2) = mothup info
        updtr ns os = ns ++ os 
