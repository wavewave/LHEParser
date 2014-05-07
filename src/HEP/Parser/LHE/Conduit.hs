{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveDataTypeable #-}

-----------------------------------------------------------------------------
-- |
-- Module      : HEP.Parser.LHE.Conduit
-- Copyright   : (c) 2010-2014 Ian-Woo Kim
--
-- License     : GPL-3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--
-- LHE parser using xml-conduit 
-- 
-----------------------------------------------------------------------------

module HEP.Parser.LHE.Conduit where

import           Control.Exception
import           Control.Monad.Base (MonadBase)
import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           Control.Monad.Primitive (PrimMonad)
import           Data.Conduit as C
import           Data.Conduit.List as CL
import           Data.Conduit.Util.Control as CU
import qualified Data.Text as T
import           Data.Typeable
import           Data.XML.Types
import           Text.XML.Stream.Render -- (renderBuilder)
-- 
import           HEP.Parser.LHE.Type
import           HEP.Parser.LHE.Text
-- 
import Prelude hiding (dropWhile, takeWhile)

data ParseEventException = ParseEventException String 
  deriving (Show, Eq, Typeable)

instance Exception ParseEventException


-- | check event starting
 
isEventStart :: Event -> Bool 
isEventStart (EventBeginElement name _) = nameLocalName name == "event" 
isEventStart _ = False

-- | check event ending 
isEventEnd :: Event -> Bool 
isEventEnd (EventEndElement name) = nameLocalName name == "event"
isEventEnd _ = False


-- | 
parseSingleEvent :: (MonadThrow m) => [Event] -> m LHEvent
parseSingleEvent ((EventContent content):_)  =
  case content of 
    ContentText txt -> return  (getEvent txt)
    _ -> throw (ParseEventException "cannot parse event") 
parseSingleEvent _ = throw (ParseEventException "cannot parse event")

-- | 
chunkLHEvent :: Monad m => Conduit Event m [Event] 
chunkLHEvent = CU.sequence action
  where action = do CU.dropWhile (not.isEventStart) 
                    CL.drop 1 
                    ev <- CU.takeWhileR (not.isEventEnd)
                    CL.drop 1 
                    return ev



-- | 
parseLHEvent :: (MonadThrow m) => Conduit [Event] m LHEvent 
parseLHEvent = CL.mapM parseSingleEvent 

-- | 
parseEvent :: (MonadThrow m) => Conduit Event m LHEvent  
parseEvent = chunkLHEvent =$= CL.filter (not.null) =$= parseLHEvent 

-- | 
parseLHEHeader :: (Monad m) => Conduit Event m Event 
parseLHEHeader = CU.takeWhile (not.isEventStart)

-- | 
textLHEHeader :: (MonadIO m, MonadThrow m, MonadBase base m, PrimMonad base) => Sink Event m [T.Text]
textLHEHeader = parseLHEHeader =$ renderText def =$ CL.consume 
  
-- because xml-conduit is not compatible with very    


