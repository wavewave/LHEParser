
import Control.Applicative hiding (many) 


import qualified Data.Attoparsec  as P
import Data.Attoparsec.Char8 -- as P8 
import qualified Data.ByteString.Char8 as B hiding (map) 

import HEP.Parser.LHEParser

-------------------------------

hline = putStrLn "---------------------------------"
main = do 
  hline
  putStrLn "This is a test of attoparsec parser."
  hline
  
  putStrLn " I am reading test.lhe " 
  
  bytestr <- B.readFile "test.lhe"

  let r = parse leshouchevent bytestr
      s = case r of 
            Partial _  -> onlyresult (feed r B.empty)
            Done _  _  -> onlyresult r
            Fail _ _ _ -> onlyresult r 
  
  putStrLn $ show s

onlyresult (Done _ r) = r
  

somestring (Fail a _ message ) = (Prelude.take 100 $ show $ B.take 100 a ) ++ " : " ++ message
somestring (Done a b ) = (Prelude.take 100 $ show $ B.take 100 a ) ++ " : " ++ (Prelude.take 100  (show b))
  



--- SOME TEST CODE , I WILL ELIMINATE THIS SOON
{-
test1 = skipSpaces *> langle

test2 :: Parser String
test2 = trim_starting_space *> (langle *> (many1 . satisfy . inClass) "abcd" <* rangle) 

h1 = do r <- header 
        trim_starting_space
        return r


test = do leshouches_starter
          trim_starting_space
          string "<header>"
          result <- headercontent 
          return result
-}