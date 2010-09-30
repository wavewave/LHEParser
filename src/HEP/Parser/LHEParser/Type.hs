module HEP.Parser.LHEParser.Type where

import qualified Data.Map as M
import qualified Data.ByteString as B
import Control.Monad.State

data LHEvent = LHEvent EventInfo [PtlInfo]
               deriving Show

type PtlID = Int 

class IDable a  where 
  idee :: a -> Int

instance IDable PtlInfo where
  idee = ptlid

data EventInfo = EvInfo { 
  nup    :: Int, 
  idprup :: Int, 
  xwgtup :: Double,
  scalup :: Double, 
  aqedup :: Double, 
  aqcdup :: Double
  } deriving Show

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

type EventReadMonadT = StateT (B.ByteString, PtlID) 


type PDGID = Int 

type PtlInfoMap = M.Map Int PtlInfo

data DecayTop a = Decay (a, [ DecayTop a ] ) 
                | Terminal a 
                deriving Eq
                                     
type DecayMap = M.Map PtlID [PtlID]
data Cross    = Cross {
  incoming :: [PtlID], 
  outgoing :: [PtlID]
  } deriving (Show,Eq)

data IntTree = IntTree {
  cross    ::  Cross,
  decaymap :: DecayMap 
  } deriving (Show,Eq)

data PtlIDInfo = PIDInfo {
  pdgid :: PDGID,  
  ptlinfo :: PtlInfo
} deriving Show

instance Functor (DecayTop) where
  fmap f (Decay (x, xs)) = Decay ((f x), map (fmap f) xs) 
  fmap f (Terminal x)    = Terminal (f x)


instance (Show a) => Show (DecayTop a) where
  show (Decay (x, xs)) = "Decay " ++ show (x,xs)
  show (Terminal x)    = "Terminal " ++ show x 


instance (Ord a) => Ord (DecayTop a) where
  compare (Decay (x,xs)) (Decay (y,ys)) = compare x y 
  compare (Terminal x) (Terminal y) = compare x y


instance Eq PtlIDInfo where
  (PIDInfo x1 y1) == (PIDInfo x2 y2) = x1 == x2 

instance Ord PtlIDInfo where
  compare (PIDInfo x1 y1) (PIDInfo x2 y2) = compare x1 x2


