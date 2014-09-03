-----------------------------------------------------------------------------
-- |
-- Module      : HEP.Parser.LHE.DecayTop
-- Copyright   : (c) 2010-2013 Ian-Woo Kim
--
-- License     : GPL-3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--
-- Decay topology parser 
-- 
-----------------------------------------------------------------------------

module HEP.Parser.LHE.DecayTop where

import           Control.Monad
import           Data.Conduit 
import qualified Data.Conduit.List as CL
import           Data.List hiding (map)
import qualified Data.Map as M
-- import           Data.Traversable (for)
-- from hep-platform 
import           HEP.Util.Functions
-- from this package 
import           HEP.Parser.LHE.Type


-- | 
findonlyTerminal :: DecayTop a -> [DecayTop a] -> [DecayTop a] 
findonlyTerminal (Terminal x) xs = (Terminal x) : xs 
findonlyTerminal (Decay (_x,xs)) ys = foldr findonlyTerminal ys xs 

-- | make an ordered decay top structure from unordered by ordering defined for the type a
mkOrdDecayTop :: (Ord a) => DecayTop a -> DecayTop a 
mkOrdDecayTop (Decay (x, xs)) = Decay (x, map (mkOrdDecayTop) (sort xs)) 
mkOrdDecayTop (Terminal x) = Terminal x

-- | 
mkDecayPDGExtTop :: PtlInfoMap -> DecayTop PtlID -> DecayTop PtlIDInfo  
mkDecayPDGExtTop pmap idtop = fmap f idtop 
  where f pid = let pinfo = maybe undefined id (M.lookup pid pmap)
                in PIDInfo (idup pinfo) pinfo

-- | 
mkDecayPDGTop :: PtlInfoMap -> DecayTop PtlID -> DecayTop PDGID
mkDecayPDGTop pmap idtop = fmap f idtop  
  where f pid = let pinfo = maybe undefined id (M.lookup pid pmap)
                in  idup pinfo

-- | 
mkFullDecayTop :: IntTree -> [DecayTop PtlID] 
mkFullDecayTop tree = let lst = outgoing (inout tree)
                          dmap = decaymap tree
                      in map (mkDecayTop dmap) lst

-- | 
mkDecayTop :: DecayMap -> PtlID -> DecayTop PtlID
mkDecayTop dmap pid = let dlist = (M.lookup pid dmap)
                      in  case dlist of 
                        Nothing -> Terminal pid 
                        Just ls -> Decay (pid, map (mkDecayTop dmap) ls)

-- | 
mkIntTree :: [PtlInfo] -> IntTree
mkIntTree = foldr mkIntTreeWkr (IntTree (InOut [] []) M.empty)

-- | this is not correct. only for madevent/pythia generated lhe file 
mkIntTreeWkr :: PtlInfo -> IntTree -> IntTree
mkIntTreeWkr info (IntTree cr dmap) 
  | st == (-1) = IntTree (InOut (newptlid : inptl) outptl) dmap 
  | st /= (-1) && newm1 `elem` [0,1,2] && newm2 `elem` [0,1,2] 
    = IntTree (InOut inptl (newptlid:outptl)) dmap 
  | st /= (-1) && newm1 == newm2 
    = let ndmap = M.insertWith updtr newm1 [newptlid] dmap
      in IntTree cr ndmap 
  | otherwise = IntTree (InOut inptl (newptlid : outptl)) dmap 

  where inptl  = incoming cr
        outptl = outgoing cr
        newptlid = ptlid info  
        st = istup info
        (newm1,newm2) = mothup info
        updtr ns os = ns ++ os 

-- | 
matchDecayTopAndGet4Momentum :: DecayTop PDGID -> DecayTop PtlIDInfo -> Maybe (DecayTop FourMomentum) 
matchDecayTopAndGet4Momentum (Terminal pid) (Terminal pinfo) = do
    guard (pid == pdgid pinfo)
    return . Terminal . pupTo4mom . pup . ptlinfo $ pinfo
matchDecayTopAndGet4Momentum (Decay (pid,xs)) (Decay (pinfo,ys)) = do
    guard (pid == pdgid pinfo)
    let allxs = permutations xs 
        rs = flip map allxs $ \xs' -> do 
               zs <- zipWithM matchDecayTopAndGet4Momentum xs' ys 
               return (Decay ((pupTo4mom . pup. ptlinfo) pinfo, zs))
    msum rs
matchDecayTopAndGet4Momentum _ _ = Nothing 

-- |
matchDecayTopGroupAndGet4Momentum :: DecayTop [PDGID] -> DecayTop PtlIDInfo -> Maybe (DecayTop FourMomentum) 
matchDecayTopGroupAndGet4Momentum (Terminal pids) (Terminal pinfo) = do
    guard (pdgid pinfo `elem` pids) 
    (return . Terminal . pupTo4mom . pup . ptlinfo) pinfo
matchDecayTopGroupAndGet4Momentum (Decay (pids,xs)) (Decay (pinfo,ys)) = do 
    guard (pdgid pinfo `elem` pids) 
    let allxs = permutations xs
        rs = flip map allxs $ \xs' -> do zs <- zipWithM matchDecayTopGroupAndGet4Momentum xs' ys 
                                         return (Decay ((pupTo4mom . pup. ptlinfo) pinfo, zs))
    msum rs
matchDecayTopGroupAndGet4Momentum _ _ = Nothing 

-- | 
getDecayTop :: LHEvent -> LHEventTop 
getDecayTop ev@(LHEvent _einfo pinfos) = 
  let pmap = M.fromList (Prelude.map (\x->(idee x,x)) pinfos)
      dtops = mkFullDecayTop (mkIntTree pinfos)
      ptlidinfotop = fmap (mkDecayPDGExtTop pmap) dtops 
  in  LHEventTop ev pmap ptlidinfotop
  

-- | 
decayTopConduit :: (Monad m) => Conduit LHEvent m LHEventTop
decayTopConduit = CL.map getDecayTop


-- | make ordered decay topology from unordered decaytop
ordDecayTopConduit :: Monad m => Conduit LHEventTop m LHEventTop 
ordDecayTopConduit = 
    CL.map (\(LHEventTop a b cs) -> LHEventTop a b (fmap mkOrdDecayTop cs))
