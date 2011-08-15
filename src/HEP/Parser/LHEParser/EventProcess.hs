{-# LANGUAGE FlexibleInstances, PackageImports #-}

module HEP.Parser.LHEParser.EventProcess where

import Control.Monad.State.Lazy

import Data.Attoparsec.Char8 hiding (takeWhile,take)
import qualified Data.ByteString.Char8 as B hiding (map) 
import Data.Maybe

import HEP.Parser.LHEParser.Type
import HEP.Parser.LHEParser.Parser

eacheventM :: State B.ByteString (Maybe LHEvent)
eacheventM = do bstr <- get
                case parse eachevent bstr of
                  Done bstr' result -> do put bstr'
                                          return result
                  _ -> return Nothing


parseevents :: B.ByteString -> [Maybe LHEvent]
parseevents bstr = takeWhile isJust $
                     evalState (sequence (repeat eacheventM)) bstr



