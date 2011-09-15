{-# LANGUAGE OverloadedStrings, NoMonomorphismRestriction #-}

module HEP.Parser.LHEParser.Parser.Enumerator where

import Control.Monad.IO.Class

import Data.XML.Types
import Data.Enumerator as E
import Data.Enumerator.List as EL
import qualified Data.Text as T

import HEP.Parser.LHEParser.Type
import HEP.Parser.LHEParser.Parser.Text

import Text.XML.Enumerator.Render

isEventStart :: Event -> Bool 
isEventStart (EventBeginElement name _) = nameLocalName name == "event" 
isEventStart _ = False

isEventEnd :: Event -> Bool 
isEventEnd (EventEndElement name) = nameLocalName name == "event"
isEventEnd _ = False

chunkAsLHEvent :: (Monad m) => Enumeratee Event [Event] m b
chunkAsLHEvent = E.sequence $ do 
                   EL.dropWhile (not.isEventStart)
                   EL.drop 1 
                   ev <- EL.takeWhile (not.isEventEnd)
                   EL.drop 1 
                   return ev
  
parseSingleEvent :: [Event] -> Maybe LHEvent
parseSingleEvent ((EventContent content):_)  =
  case content of 
    ContentText txt -> Just (getEvent txt)
    _ -> Nothing
parseSingleEvent _ = Nothing

parseSingleEventIter :: (Monad m) => Enumeratee [Event] (Maybe LHEvent) m a
parseSingleEventIter = EL.map parseSingleEvent 

parseEventIter :: (Monad m) => Iteratee (Maybe LHEvent) m a -> Iteratee Event m a 
parseEventIter x = chunkAsLHEvent =$ EL.filter (not.null) =$ parseSingleEventIter =$ x


parseLHEHeader :: (Monad m) => Iteratee Event m [Event] 
parseLHEHeader = do 
  evs <- EL.takeWhile (not.isEventStart)
  return evs 

textLHEHeader :: (MonadIO m) => Iteratee Event m [T.Text]
textLHEHeader = do 
  headevs <- parseLHEHeader 
  run_ $ E.enumList 1 headevs $$ renderText =$ EL.consume
