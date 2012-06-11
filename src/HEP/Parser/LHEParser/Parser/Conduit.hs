{-# LANGUAGE OverloadedStrings, NoMonomorphismRestriction #-}

module HEP.Parser.LHEParser.Parser.Conduit where

import Control.Monad.IO.Class

import Data.XML.Types
import Data.Conduit as C
import Data.Conduit.List as CL
import qualified Data.Text as T

import HEP.Parser.LHEParser.Type
import HEP.Parser.LHEParser.Parser.Text

import Text.XML.Stream.Render

import Prelude hiding (dropWhile, takeWhile)

-- | dropWhile for Listlike conduit 

dropWhile :: Monad m => (a -> Bool) -> Sink a m () 
dropWhile p = 
    NeedInput push close 
  where
    push b | p b = dropWhile p 
           | otherwise = Done Nothing ()
    close = return ()  
     
-- | takeWhile for Listlike conduit

takeWhile :: Monad m => (a -> Bool) -> Sink a m [a] 
takeWhile p = go p id 
  where 
    go p front = NeedInput (push p front) (return $ front [])

    push p front x 
        | p x = NeedInput (push p front') (return $ front' [])
        | otherwise = Done Nothing (front [])
      where front' = front . (x:) 


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

chunkLHEventConduit :: (Monad m) => Conduit Event m [Event] 
chunkLHEventConduit = C.sequence $ do 
                        dropWhile (not.isEventStart)
                        CL.drop 1 
                        ev <- takeWhile (not.isEventEnd)
                        CL.drop 1 
                        return ev

-- | 

parseLHEventConduit :: (Monad m) => Conduit [Event] m (Maybe LHEvent) 
parseLHEventConduit = CL.map parseSingleEvent 

-- | 

parseEventConduit :: (Monad m) => Sink (Maybe LHEvent) m a -> Sink Event m a 
parseEventConduit x = chunkLHEventConduit =$ CL.filter (not.null) =$ parseLHEventConduit =$ x

-- | 

parseLHEHeader :: (Monad m) => Sink Event m [Event] 
parseLHEHeader = do 
  evs <- takeWhile (not.isEventStart)
  return evs 


{- 
textLHEHeader :: (MonadIO m) => Sink Event m [T.Text]
textLHEHeader = do 
  headevs <- parseLHEHeader 
  run_ $ C.enumList 1 headevs $$ renderText def =$ CL.consume
-}
