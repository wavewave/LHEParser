{-# LANGUAGE FlexibleInstances, PackageImports #-}

module HEP.Parser.LHEParser.EventProcess where

import qualified Data.Attoparsec  as P
import Data.Attoparsec.Char8 hiding (takeWhile,take)
import qualified Data.ByteString.Char8 as B hiding (map) 

import HEP.Parser.LHEParser.Type
import HEP.Parser.LHEParser.Parser

import Data.List 
import Data.Maybe

import Control.Monad.State.Lazy


eacheventM :: State B.ByteString (Maybe LHEvent)
eacheventM = do bstr <- get
--                let Done bstr' result = 
                case parse eachevent bstr of
                  Done bstr' result -> do put bstr'
                                          return result
                  _ -> return Nothing

parseevents bstr = takeWhile isJust $
                     evalState (sequence (repeat eacheventM)) bstr

