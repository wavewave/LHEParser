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
