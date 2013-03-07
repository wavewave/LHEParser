{-# LANGUAGE OverloadedStrings, NoMonomorphismRestriction, ScopedTypeVariables #-}

-----------------------------------------------------------------------------
-- |
-- Module      : HEP.Parser.LHE.Conduit
-- Copyright   : (c) 2010-2013 Ian-Woo Kim
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

import           Control.Monad.IO.Class
import           Data.Conduit as C
import           Data.Conduit.List as CL
import           Data.Conduit.Util.Control as CU
import qualified Data.Text as T
import           Data.XML.Types
import           Text.XML.Stream.Render
-- 
import           HEP.Parser.LHE.Type
import           HEP.Parser.LHE.Text
-- 
import Prelude hiding (dropWhile, takeWhile)


-- | check event starting
 
isEventStart :: Event -> Bool 
isEventStart (EventBeginElement name _) = nameLocalName name == "event" 
isEventStart _ = False

-- | check event ending 
isEventEnd :: Event -> Bool 
isEventEnd (EventEndElement name) = nameLocalName name == "event"
isEventEnd _ = False


-- | 
parseSingleEvent :: [Event] -> Maybe LHEvent
parseSingleEvent ((EventContent content):_)  =
  case content of 
    ContentText txt -> Just (getEvent txt)
    _ -> Nothing
parseSingleEvent _ = Nothing

-- | 
chunkLHEvent :: Monad m => Conduit Event m [Event] 
chunkLHEvent = CU.sequence action
  where action = do CU.dropWhile (not.isEventStart) 
                    CL.drop 1 
                    ev <- CU.takeWhileR (not.isEventEnd)
                    CL.drop 1 
                    return ev



-- | 
parseLHEvent :: (Monad m) => Conduit [Event] m (Maybe LHEvent) 
parseLHEvent = CL.map parseSingleEvent 

-- | 
parseEvent :: (Monad m) => Conduit Event m (Maybe LHEvent)  
parseEvent = chunkLHEvent =$= CL.filter (not.null) =$= parseLHEvent 

-- | 
parseLHEHeader :: (Monad m) => Conduit Event m Event 
parseLHEHeader = CU.takeWhile (not.isEventStart)

-- | 
textLHEHeader :: (MonadIO m, MonadThrow m, MonadUnsafeIO m) => Sink Event m [T.Text]
textLHEHeader = parseLHEHeader =$ renderText def =$ CL.consume 
  
  

