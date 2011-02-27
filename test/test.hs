{-# LANGUAGE PackageImports #-}

import Control.Applicative hiding (many) 


import qualified Data.Attoparsec  as P
import Data.Attoparsec.Char8 -- as P8 
import qualified Data.ByteString.Char8 as B hiding (map) 

import HEP.Parser.LHEParser

import Debug.Trace


import qualified Data.Iteratee as Iter
import qualified Data.ListLike as LL 

iter_parseoneevent :: Iter.Iteratee B.ByteString IO (Maybe LHEvent)
iter_parseoneevent = Iter.Iteratee step 
  where step = undefined 




-------------------------------

hline = putStrLn "---------------------------------"
main = do 
  hline
  putStrLn "This is a test of attoparsec parser."
  hline
  
  putStrLn " I am reading test.lhe " 
  
  bytestr <- B.readFile "test.lhe"

  let r = parse lheheader bytestr
      s = case r of 
            Partial _  -> onlyremain (feed r B.empty)
            Done _  _  -> onlyremain r
            Fail _ _ _ -> trace "Test failed" $ onlyremain r 
  
  putStrLn $ show $ B.take 100 s

  let r' = parse eachevent s
      s' = case r' of 
            Partial _  -> onlyresult (feed r' B.empty)
            Done _  _  -> onlyresult r'
            Fail _ _ _ -> trace "Test failed" $ onlyresult r'
            
  putStrLn $ show s'
  

        
onlyresult (Done _ r) = r
onlyremain (Done s _) = s
  

somestring (Fail a _ message ) = (Prelude.take 100 $ show $ B.take 100 a ) ++ " : " ++ message
somestring (Done a b ) = (Prelude.take 100 $ show $ B.take 100 a ) ++ " : " ++ (Prelude.take 100  (show b))
  

