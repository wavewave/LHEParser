{-# LANGUAGE OverloadedStrings #-}

import Control.Applicative hiding (many) 

import qualified Data.Attoparsec  as P
import Data.Attoparsec.Char8 -- as P8 
import qualified Data.ByteString.Char8 as B hiding (map) 
--import qualified Data.ByteString as B hiding (map)

skipSpaces :: Parser () 
skipSpaces = P.satisfy isHorizontalSpace *> P.skipWhile isHorizontalSpace


trim_starting_space :: Parser () 
trim_starting_space = do try endOfInput 
                         <|> (many . satisfy . inClass ) " \n" *> return () 
--                         try endOfInput
--                         <|> return ()

langle :: Parser Char
langle = char '<'

rangle :: Parser Char 
rangle = char '>'

leshouches_starter = do string "<LesHouchesEvents" 
                        (many . satisfy . notInClass) ">"
                        char '>'
                        

mymaybeWhile :: Parser (Maybe a) -> Parser [a]
mymaybeWhile oneelem = do x <- oneelem
                          case x of 
                            Just t -> do ts <- mymaybeWhile oneelem
                                         return (t:ts)  
                            Nothing-> return []


header :: Parser String
header = do string "<header>"  
            result <- headercontent
            return result
            

  
         
oneelem = try (string "</header>" >> return Nothing) 
          <|> (many1 (notChar '<') >>= return.Just )
          <|> do a <- anyChar 
                 return (Just [a])
            
headercontent :: Parser String
headercontent = mymaybeWhile oneelem >>= return . concat
  


withintag :: B.ByteString -> B.ByteString -> Parser String 
withintag stag etag = do string stag
                         result <- withintageach etag 
                         string etag
                         return result

withintageach tag = try (string tag >> return "" ) 
                    <|> (many1 (notChar '<'))

{-
initevent :: Parser String 
initevent = do string "<init>"
               result <- initcontent
               string "</init>"
               return result
initcontent = try (string "</init>" >> return "" ) 
              <|> (many1 (notChar '<'))
-}
initevent = withintag "<init>" "</init>"

eachevent = withintag "<event>" "</event>"

  
  
  --many ( notChar '<' 
  --                      <|> try (string "</header>" >> return  )
 --                   <|> (do a <- char '<' 
 --                           return [a] )
--                    return $ s: r 
--                    try (string "</header>" >> return "")
--                    return r
--                        <|> do a <- anyChar 
--                               return [a]     
                                 --do h <- headercontent
                                    --   return ('<' : h)
  
  
  
  
--  
 -- do try (string "</header>" >> return "") 
 --                  <|> do c <- anyChar
 --                         h <- headercontent
  --                        return (c:h)
                
  
  
 --                  <|> do c <- char '<' 
 --                         h <- headercontent
 --                         return (c: h )


test1 = skipSpaces *> langle

test2 :: Parser String
test2 = trim_starting_space *> (langle *> (many1 . satisfy . inClass) "abcd" <* rangle) 

--test = many1 (header <* try skipSpaces) 
--test = do trim_starting_space  
 --         many1 (header <* trim_starting_space )
h1 = do r <- header 
        trim_starting_space
        return r


leshouchevent :: Parser [String]
leshouchevent = do leshouches_starter
                   trim_starting_space
                   h <- header
                   trim_starting_space
                   ini <- initevent 
                   trim_starting_space
                   r <- many1 (eachevent <* trim_starting_space)
                   return r

test = do leshouches_starter
          trim_starting_space
          string "<header>"
          result <- headercontent 
          return result


--test :: Parser [String]
--test = do trim_starting_space
--          r <- many1 (header <* trim_starting_space )
--          endOfInput
--          return r

--          r1 <- h1
--          return r1
--         r2 <- h1
  --     return [r1,r2]
    --      result <- many1 h1 --(header <* trim_starting_space)
    --      return result


hline = putStrLn "---------------------------------"
main = do 
  hline
  putStrLn "This is a test of attoparsec parser."
  hline
  
  putStrLn " I am reading test.lhe " 
  
  bytestr <- B.readFile "test.lhe"
--  let bytestr = "<LesHouchesEvents version=\"1.0\"> <header>abcd <>dssd</header> "

--  let a = parseWith :: String

  let r = parse {-test-} leshouchevent bytestr -- leshouchevent bytestr
      s = case r of 
            Partial _  -> onlyresult (feed r B.empty)
            Done _  _  -> onlyresult r
            Fail _ _ _ -> onlyresult r 
  
  putStrLn $ show $ Prelude.length s


onlyresult (Done _ r) = r
  
somestring (Fail a _ message ) = (Prelude.take 100 $ show $ B.take 100 a ) ++ " : " ++ message
somestring (Done a b ) = (Prelude.take 100 $ show $ B.take 100 a ) ++ " : " ++ (Prelude.take 100  (show b))
  
--"\n \n <header> abcd </header>  <header> dkddk </header>  " 
--  parseTest strange " a"