{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeSynonymInstances #-}

-----------------------------------------------------------------------------
-- |
-- Module      : HEP.Parser.LHE.Type
-- Copyright   : (c) 2010-2013 Ian-Woo Kim
--
-- License     : GPL-3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--
-- Types for LHEParser library
--
-----------------------------------------------------------------------------


module HEP.Parser.LHE.Type where

import           Control.Monad.State
import qualified Data.ByteString as B
import           Data.Foldable
import           Data.Functor
import           Data.Traversable
import qualified Data.Map as M
--
import           HEP.Util.Functions

-- |

data LHEvent = LHEvent EventInfo [PtlInfo]
               deriving Show

-- |
type PtlID = Int 

-- | 
class IDable a  where 
  idee :: a -> Int

-- | 
instance IDable PtlInfo where
  idee = ptlid

-- |
data EventInfo = EvInfo { 
  nup    :: Int, 
  idprup :: Int, 
  xwgtup :: Double,
  scalup :: Double, 
  aqedup :: Double, 
  aqcdup :: Double
  } deriving Show

-- | reference : <http://lcgapp.cern.ch/project/docs/lhef5.pdf>
data PtlInfo   = PtlInfo {
  ptlid  :: PtlID, 
  idup   :: Int, 
  istup  :: Int,
  mothup :: (Int,Int), 
  icolup :: (Int,Int), 
  pup    :: (Double, Double, Double, Double, Double), 
  vtimup :: Double, 
  spinup :: Double
  } deriving Show

-- | 
emptyPtlInfo :: PtlInfo
emptyPtlInfo = PtlInfo { ptlid = 0
                       , idup  = 0
                       , istup = 0
                       , mothup = (0,0)
                       , icolup = (0,0)
                       , pup = (0,0,0,0,0)
                       , vtimup = 0
                       , spinup = 0 }

-- | 

type EventReadMonadT = StateT (B.ByteString, PtlID) 

-- | 

type PDGID = Int 

-- | 
type PtlInfoMap = M.Map Int PtlInfo

-- | 
data DecayTop a = Decay (a, [ DecayTop a ] ) 
                | Terminal a 
                deriving Eq
     
-- | 
type DecayMap = M.Map PtlID [PtlID]

-- | 
data InOut  = InOut {
  incoming :: [PtlID], 
  outgoing :: [PtlID]
  } deriving (Show,Eq)

-- | 
data IntTree = IntTree {
  inout    ::  InOut,
  decaymap :: DecayMap 
} deriving (Show,Eq)

-- | 
data Collision 

-- | 
data Decay 

-- | 
data ProcessTree a b where 
  CrossTree :: (b,b) -> DecayTop b -> ProcessTree Collision b
  DecayTree :: b -> DecayTop b -> ProcessTree Decay b

-- | get the pair of incoming particles for cross process
incomingPair :: ProcessTree Collision b -> (b,b)
incomingPair (CrossTree (x,y) _) = (x,y)

-- | get the mother particle for decay process

motherPtl :: ProcessTree Decay b -> b 
motherPtl (DecayTree x _) = x

-- |

data EventTree b = CrossEvent (ProcessTree Collision b)
                 | DecayEvent (ProcessTree Decay b)

-- | 

isCrossEvent :: EventTree b -> Bool
isCrossEvent (CrossEvent _) = True
isCrossEvent _ = False

-- | 

isDecayEvent :: EventTree b -> Bool 
isDecayEvent (DecayEvent _) = True 
isDecayEvent _ = False

-- | 

data PtlIDInfo = PIDInfo {
  pdgid :: PDGID,  
  ptlinfo :: PtlInfo
} deriving Show

class GetPDGID a where
  getPDGID :: a -> PDGID

instance GetPDGID PDGID where
  getPDGID = id

instance GetPDGID PtlIDInfo where
  getPDGID = pdgid

-- | 
{-
instance Functor (DecayTop) where
  fmap f (Decay (x, xs)) = Decay ((f x), map (fmap f) xs) 
  fmap f (Terminal x)    = Terminal (f x)
-}
-- | 

instance (Show a) => Show (DecayTop a) where
  show (Decay (x, xs)) = "Decay " ++ show (x,xs)
  show (Terminal x)    = "Terminal " ++ show x 

-- | 

instance (Ord a) => Ord (DecayTop a) where
  compare (Decay (x,xs)) (Decay (y,ys)) = case compare x y of 
                                            GT -> GT
                                            LT -> LT
                                            EQ -> compare xs ys
  compare (Terminal _) (Decay _) = LT
  compare (Decay _) (Terminal _) = GT
  compare (Terminal x) (Terminal y) = compare x y
  -- compare _ _ = undefined 

-- | 

instance Eq PtlIDInfo where
  (PIDInfo x1 _y1) == (PIDInfo x2 _y2) = x1 == x2 

-- | 

instance Ord PtlIDInfo where
  compare (PIDInfo x1 _y1) (PIDInfo x2 _y2) = compare x1 x2

-- | 

pupTo4mom :: (Double,Double,Double,Double,Double) -> FourMomentum 
pupTo4mom (x,y,z,e,_m) = (e,x,y,z)


data LHEventTop = LHEventTop { lhet_ev :: LHEvent
                             , lhet_pinfos :: PtlInfoMap 
                             , lhet_dtops :: [DecayTop PtlIDInfo]
                             } 
                deriving (Show)

deriving instance Functor DecayTop

deriving instance Foldable DecayTop

deriving instance Traversable DecayTop
