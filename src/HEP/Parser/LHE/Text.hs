{-# LANGUAGE OverloadedStrings, BangPatterns, ScopedTypeVariables #-}

-----------------------------------------------------------------------------
-- |
-- Module      : HEP.Parser.LHE.Text
-- Copyright   : (c) 2010-2013 Ian-Woo Kim
--
-- License     : GPL-3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--
-- number parsing
-- 
-----------------------------------------------------------------------------


module HEP.Parser.LHE.Text where


import Control.Monad.Identity 
import Control.Monad.State
import Data.Text as T
import Data.Text.Read
-- 
import HEP.Parser.LHE.Type
--
import Prelude hiding (head,lines,unlines)

type EventReadMonadTextT = StateT (Text, PtlID)


getText :: (Monad m) => EventReadMonadTextT m Text
getText = get >>= return . fst 

putText :: (Monad m) => Text -> EventReadMonadTextT m ()
putText t = do (_,i) <- get 
               put (t,i)

getID  :: Monad m => EventReadMonadTextT m Int
getID = do (_,i) <- get 
           return i
          
putID  :: Monad m => Int -> EventReadMonadTextT m ()
putID i = do (s,_) <- get
             put (s,i)

incID  :: Monad m => EventReadMonadTextT m () 
incID = do i <- getID
           putID (i+1)

skipWhite :: (Monad m) => EventReadMonadTextT m () 
skipWhite = getText >>= putText.stripStart
               
dotElem :: (Monad m) => EventReadMonadTextT m ()
dotElem = do s <- getText 
             when (not (T.null s)) $
               when (T.head s == '.') $
                 putText (T.tail s)    


readM :: (Monad m) => Reader a -> EventReadMonadTextT m a 
readM f = do s <- getText 
             case f s of 
               Left err -> error (err ++ " : " ++ show s )  -- return (Left err)
               Right (a,s') -> do 
                 putText s'
                 return a


readEvCommon :: EventReadMonadTextT Identity EventInfo
readEvCommon = do skipWhite
                  nup'    <- readM (signed decimal)
                  skipWhite
                  idprup' <- readM (signed decimal)
                  skipWhite
                  xwgtup' <- readM (signed double)
                  skipWhite
                  scalup' <- readM (signed double)
                  skipWhite
                  aqedup' <- readM (signed double)
                  skipWhite
                  aqcdup' <- readM (signed double)
                  skipWhite
               
                  return EvInfo { 
                    nup    = nup', 
                    idprup = idprup', 
                    xwgtup = xwgtup',
                    scalup = scalup', 
                    aqedup = aqedup', 
                    aqcdup = aqcdup'
                    }


readEvPtl :: EventReadMonadTextT Identity PtlInfo
readEvPtl = do nid <- getID
    
               idup'    <- readM (signed decimal)
               skipWhite
               istup'   <- readM (signed decimal)
               skipWhite
               mothup1' <- readM (signed decimal)
               skipWhite
               mothup2' <- readM (signed decimal)
               skipWhite
               icolup1' <- readM (signed decimal)
               skipWhite
               icolup2' <- readM (signed decimal)
               skipWhite
               
               pup1' <- readM (signed double)
               dotElem
               skipWhite
               pup2' <- readM (signed double)
               dotElem
               skipWhite
               pup3' <- readM (signed double)
               dotElem
               skipWhite
               pup4' <- readM (signed double)
               dotElem
               skipWhite
               pup5' <- readM (signed double)
               dotElem
               skipWhite
               vtimup' <- readM (signed double)
               dotElem
               skipWhite               
               spinup' <- readM (signed double)
               dotElem
               skipWhite
             
               incID
               
               return PtlInfo { 
                 ptlid  = nid,
                 idup   = idup'  , 
                 istup  = istup' ,
                 mothup = (mothup1',mothup2'), 
                 icolup = (icolup1',icolup2'), 
                 pup    = (pup1',pup2',pup3',pup4',pup5'), 
                 vtimup = vtimup', 
                 spinup = spinup'
                 }


readEvWkr :: EventReadMonadTextT Identity [PtlInfo]
readEvWkr = do s <- getText 
               if T.null s  
                  then return [] 
                  else do x  <- readEvPtl 
                          xs <- readEvWkr                          
                          return (x : xs)


readEvent :: EventReadMonadTextT Identity LHEvent
readEvent = do evinfo   <- readEvCommon
               ptlinfos <- readEvWkr
               return $ LHEvent evinfo ptlinfos
             

getEvent :: Text -> LHEvent
getEvent txt = let txt' = removeComment txt 
               in  runIdentity $ evalStateT readEvent (txt',1)


removeComment :: Text -> Text 
removeComment txt = let (txts :: [Text]) = lines txt
                        (notnulltxts :: [Text]) = Prelude.filter (not. T.null) txts
                        filtered = Prelude.filter (\x->T.head x /= '#')  notnulltxts
                    in  unlines filtered 
  
  
