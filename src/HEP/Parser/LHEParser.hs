{-# LANGUAGE OverloadedStrings, BangPatterns #-}

module HEP.Parser.LHEParser where

import Control.Applicative hiding (many) 

import qualified Data.Attoparsec  as P
import Data.Attoparsec.Char8 -- as P8 
import qualified Data.ByteString.Char8 as B hiding (map) 
--import qualified Data.ByteString as B hiding (map)

import Data.ByteString.Lex.Double

import Control.Monad.State

import Debug.Trace

data LHEvent = LHEvent EventInfo [PtlInfo]
               deriving Show

data EventInfo = EvInfo { 
  nup    :: Int, 
  idprup :: Int, 
  xwgtup :: Double,
  scalup :: Double, 
  aqedup :: Double, 
  aqcdup :: Double
  } deriving Show

data PtlInfo   = PtlInfo {
  idup   :: Int, 
  istup  :: Int,
  mothup :: (Int,Int), 
  icolup :: (Int,Int), 
  pup    :: (Double, Double, Double, Double, Double), 
  vtimup :: Double, 
  spinup :: Double
  } deriving Show


skipSpaces :: Parser () 
skipSpaces = P.satisfy isHorizontalSpace *> P.skipWhile isHorizontalSpace


trim_starting_space :: Parser () 
trim_starting_space = do try endOfInput 
                         <|> (many . satisfy . inClass ) " \n" *> return () 

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
  
withintag :: B.ByteString -> B.ByteString -> (B.ByteString -> a) -> Parser a
withintag stag etag f = string stag *> (f <$> withintageach etag ) <* string etag
--                           return (f result

withintageach tag = try (string tag >> return B.empty ) 
                    <|> (takeWhile1 (/= '<'))



initevent = withintag "<init>" "</init>" show

eachevent = withintag "<event>" "</event>" getEvent

leshouchevent :: Parser [Maybe LHEvent]
leshouchevent = do leshouches_starter
                   trim_starting_space
                   h <- header
                   trim_starting_space
                   ini <- initevent 
                   trim_starting_space
                   r <- many1 (eachevent <* trim_starting_space)
                   return r

-------------------------------

type EventReadMonadT = StateT B.ByteString 

isWhite :: Char -> Bool 
isWhite c = if c == ' ' || c == '\n' 
            then True
            else False

consume1 :: EventReadMonadT Maybe () 
consume1 = do s <- get 
         --     trace ("hey " ++ show s) $ 
              put (B.tail s)

skipWhite :: EventReadMonadT Maybe () 
skipWhite = do s <- get 
               let s' = B.dropWhile isWhite s
               put s'
               
readIntM :: EventReadMonadT Maybe Int  
readIntM = do s <- get 
              (i,s') <- (lift . B.readInt) s
              put s'
              return i

readDoubleM :: EventReadMonadT Maybe Double
readDoubleM = do s <- get 
                 (r,s') <- (lift . readDouble) s
                 put s' 
                 if B.head s' == '.' 
                   then consume1 
                   else return ()  -- trace "not consumed" $ put s'
                 return r

readEvCommon :: EventReadMonadT Maybe EventInfo
readEvCommon = do skipWhite
                  nup'    <- readIntM  
                  skipWhite
                  idprup' <- readIntM 
                  skipWhite
                  xwgtup' <- readDoubleM
                  skipWhite
                  scalup' <- readDoubleM
                  skipWhite
                  aqedup' <- readDoubleM
                  skipWhite
                  aqcdup' <- readDoubleM
                  skipWhite
               
                  return EvInfo { 
                    nup    = nup', 
                    idprup = idprup', 
                    xwgtup = xwgtup',
                    scalup = scalup', 
                    aqedup = aqedup', 
                    aqcdup = aqcdup'
                    }


readEvPtl :: EventReadMonadT Maybe PtlInfo
readEvPtl = do idup'    <- readIntM
               skipWhite
               istup'   <- readIntM 
               skipWhite
               mothup1' <- readIntM
               skipWhite
               mothup2' <- readIntM
               skipWhite
               icolup1' <- readIntM
               skipWhite
               icolup2' <- readIntM 
               skipWhite
               
               pup1' <- readDoubleM
               skipWhite
               pup2' <- readDoubleM
               skipWhite
               pup3' <- readDoubleM
               skipWhite
               pup4' <- readDoubleM
               skipWhite
               pup5' <- readDoubleM
               skipWhite
               vtimup' <- readDoubleM
               skipWhite               
               spinup' <- readDoubleM
               skipWhite
             
               return PtlInfo { 
                 idup   = idup'  , 
                 istup  = istup' ,
                 mothup = (mothup1',mothup2'), 
                 icolup = (icolup1',icolup2'), 
                 pup    = (pup1',pup2',pup3',pup4',pup5'), 
                 vtimup = vtimup', 
                 spinup = spinup'
                 }

{-untilM :: (Monad m) => (a -> Bool) -> (a -> m a) -> a -> m a 
untilM p f x | p x       = return x 
             | otherwise = f x >> untilM p f -}

readEvWkr = do s <- get 
               if B.null s  
                  then return [] 
                  else do x  <- readEvPtl 
                          xs <- readEvWkr                          
                          return (x : xs)


readEvent :: EventReadMonadT Maybe LHEvent
readEvent = do evinfo   <- readEvCommon
               ptlinfos <- readEvWkr
               return $ LHEvent evinfo ptlinfos
             

getEvent :: B.ByteString -> Maybe LHEvent
getEvent bs = evalStateT readEvent bs


