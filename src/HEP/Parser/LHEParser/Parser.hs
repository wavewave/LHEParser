{-# LANGUAGE OverloadedStrings, BangPatterns #-}

module HEP.Parser.LHEParser.Parser where

import HEP.Parser.LHEParser.Type
 
import Control.Applicative hiding (many) 



import qualified Data.Attoparsec  as P
import Data.Attoparsec.Char8 -- as P8 
--import Data.Attoparsec.Internal
--import qualified Data.ByteString as B hiding (map)


import qualified Data.ByteString.Char8 as B hiding (map) 

--import Text.Parsec

import Data.ByteString.Lex.Double

import Control.Monad.State

import Debug.Trace

import qualified Data.Map as M
--import qualified Data.Iteratee as Iter
--import qualified Data.ListLike as LL

{-
lookAhead :: Parser a -> Parser a 
lookAhead p = Parser $ \st0 kf ks -> 
              runParser p (noAdds st0) (kf . mappend st0) ks
-}
    

---- ID Map 

cnstrctIDMap :: (IDable a) =>  [a] -> M.Map Int a
cnstrctIDMap vs = foldr f M.empty vs
  where f v acc = M.insert (idee v) v acc

---- parsers 

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

oneevent :: Parser (Maybe LHEvent) 
oneevent = withintag "<event>" "</event>" getEvent

eachevent :: Parser (Maybe LHEvent)
eachevent = oneevent <* trim_starting_space


lheheader :: Parser () -- Int --- (Maybe [LHEvent])
lheheader = do leshouches_starter
               trim_starting_space
               header
               trim_starting_space
               initevent 
               trim_starting_space
               return ()
               
        --           r <- many1 (fromJust <$> eachevent <* trim_starting_space)
        --           let r' = length r -- sequence r 
        --           return r

untilfirstevent :: Parser () 
untilfirstevent = do skipWhile (/= '<')
                     (try (string "<event>" >>  return ())
                      <|> do char '<' 
                             untilfirstevent)


-------------------------------

getBStr :: (Monad m) => EventReadMonadT m B.ByteString
getBStr = do (s,_) <- get 
             return s 
          
putBStr :: Monad m => B.ByteString -> EventReadMonadT m ()
putBStr s = do (_,i) <- get 
               put (s,i)

getID  :: Monad m => EventReadMonadT m Int
getID = do (_,i) <- get 
           return i
          
putID  :: Monad m => Int -> EventReadMonadT m ()
putID i = do (s,_) <- get
             put (s,i)

incID  :: Monad m => EventReadMonadT m () 
incID = do i <- getID
           putID (i+1)


isWhite :: Char -> Bool 
isWhite c = if c == ' ' || c == '\n' 
            then True
            else False

consume1 :: EventReadMonadT Maybe () 
consume1 = do s <- getBStr 
         --     trace ("hey " ++ show s) $ 
              putBStr (B.tail s)

skipWhite :: EventReadMonadT Maybe () 
skipWhite = do s <- getBStr 
               let s' = B.dropWhile isWhite s
               putBStr s'
               
readIntM :: EventReadMonadT Maybe Int  
readIntM = do s <- getBStr 
              (i,s') <- (lift . B.readInt) s
              putBStr s'
              return i

readDoubleM :: EventReadMonadT Maybe Double
readDoubleM = do s <- getBStr 
                 (r,s') <- (lift . readDouble) s
                 putBStr s' 
                 if B.head s' == '.' 
                   then consume1 
                   else return ()  -- trace "not consumed" $ put s'
                 -- trace $ "r = " ++ (show r) $ 
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
readEvPtl = do nid <- getID
    
               idup'    <- readIntM
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
             
               incID
               
               return PtlInfo { 
                 ptlid  = nid,
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

readEvWkr :: EventReadMonadT Maybe [PtlInfo]
readEvWkr = do s <- getBStr 
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
getEvent bs = let bs' = removeComment bs 
              in  evalStateT readEvent (bs',1)


removeComment :: B.ByteString -> B.ByteString 
removeComment bstr = let bstrs = B.lines bstr
                         notnullbstrs = filter (not. B.null) bstrs
                         filtered = filter (\x->B.head x /= '#')  notnullbstrs
                     in  B.unlines filtered 
  
  
