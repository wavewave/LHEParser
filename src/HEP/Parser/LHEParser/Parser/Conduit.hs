{-# LANGUAGE OverloadedStrings, NoMonomorphismRestriction, ScopedTypeVariables #-}

module HEP.Parser.LHEParser.Parser.Conduit where

import Control.Monad.IO.Class

import Data.XML.Types
import Data.Conduit as C
import Data.Conduit.List as CL
import Data.Conduit.Util.Control as CU
import qualified Data.Text as T

import HEP.Parser.LHEParser.Type
import HEP.Parser.LHEParser.Parser.Text

import Text.XML.Stream.Render

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
 
chunkLHEvent :: forall m u. Monad m => Conduit Event m [Event] 
chunkLHEvent = CU.sequence action
  where action :: Sink Event m [Event] 
        action = do CU.dropWhile (not.isEventStart) 
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
  
  

