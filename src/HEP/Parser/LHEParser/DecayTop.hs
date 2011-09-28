module HEP.Parser.LHEParser.DecayTop where

import HEP.Parser.LHEParser.Type
import qualified Data.Map as M
import Data.List hiding (map)
import Control.Monad
import HEP.Util.Functions

import Data.Enumerator hiding (map)
import qualified Data.Enumerator.List as EL


findonlyTerminal :: DecayTop a -> [DecayTop a] -> [DecayTop a] 
findonlyTerminal (Terminal x) xs = (Terminal x) : xs 
findonlyTerminal (Decay (_x,xs)) ys = foldr findonlyTerminal ys xs 


mkOrdDecayTop :: (Ord a) => DecayTop a -> DecayTop a 
mkOrdDecayTop (Decay (x, xs)) = Decay (x, map (mkOrdDecayTop) (sort xs)) 
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
mkIntTreeWkr info (IntTree cr dmap)  
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


matchDecayTopAndGet4Momentum :: DecayTop PDGID -> DecayTop PtlIDInfo -> Maybe (DecayTop FourMomentum) 
matchDecayTopAndGet4Momentum (Terminal pid) (Terminal pinfo) 
  | pid == pdgid pinfo = Just . Terminal . pupTo4mom . pup . ptlinfo $ pinfo
  | otherwise = Nothing
matchDecayTopAndGet4Momentum (Decay (pid,xs)) (Decay (pinfo,ys)) 
  | pid == pdgid pinfo = do zs <- zipWithM matchDecayTopAndGet4Momentum xs ys 
                            return (Decay ((pupTo4mom . pup. ptlinfo) pinfo, zs))
  | otherwise = Nothing
matchDecayTopAndGet4Momentum _ _ = Nothing 

matchDecayTopGroupAndGet4Momentum :: DecayTop [PDGID] -> DecayTop PtlIDInfo -> Maybe (DecayTop FourMomentum) 
matchDecayTopGroupAndGet4Momentum (Terminal pids) (Terminal pinfo) =
  if (pdgid pinfo `elem` pids) then Just . Terminal . pupTo4mom . pup . ptlinfo $ pinfo
                               else Nothing
matchDecayTopGroupAndGet4Momentum (Decay (pids,xs)) (Decay (pinfo,ys)) = 
  if (pdgid pinfo `elem` pids) then do zs <- zipWithM matchDecayTopGroupAndGet4Momentum xs ys 
                                       return (Decay ((pupTo4mom . pup. ptlinfo) pinfo, zs))
                               else Nothing
matchDecayTopGroupAndGet4Momentum _ _ = Nothing 

getDecayTop :: LHEvent -> (LHEvent,PtlInfoMap,[DecayTop PtlIDInfo])
getDecayTop ev@(LHEvent _einfo pinfos) = 
  let pmap = M.fromList (Prelude.map (\x->(idee x,x)) pinfos)
      dtops = mkFullDecayTop (mkIntTree pinfos)
      ptlidinfotop = fmap (mkDecayPDGExtTop pmap) dtops 
  in  (ev,pmap,ptlidinfotop)
  
decayTopEnee :: (Monad m) => Enumeratee (Maybe LHEvent) (Maybe (LHEvent,PtlInfoMap,[DecayTop PtlIDInfo])) m a
decayTopEnee = EL.map (fmap getDecayTop)  


-- | make ordered decay topology from unordered decaytop
ordDecayTopEnee :: Monad m => 
                   Enumeratee (Maybe (LHEvent,PtlInfoMap,[DecayTop PtlIDInfo]))
                              (Maybe (LHEvent,PtlInfoMap,[DecayTop PtlIDInfo]))
                              m a
ordDecayTopEnee = EL.map (fmap f)
  where f (a,b,cs) = (a,b,Prelude.map mkOrdDecayTop cs)
