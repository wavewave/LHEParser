{-# LANGUAGE ForeignFunctionInterface #-}

module Main where 

import HEP.Parser.LHEParser.Type
import HEP.Parser.LHEParser.Formatter

main :: IO ()
main = do 
  putStrLn "haha"
  putStrLn $ formatEventInfo $ EvInfo 2 2 4 4 4 4  
  putStrLn $ formatParticleInfo $ PtlInfo 1 8 32 (3,0) (4,4) (3.2,233.3,1010.0,0.332,0.0003) 2.0 1.0
{-  let lst1 = unsafePerformIO $ newArray (map fromIntegral [3,0])
      lst2 = unsafePerformIO $ newArray (map fromIntegral [4,4])
      lst3 = unsafePerformIO $ newArray (map realToFrac [3.2,233.3,1010.0,0.332,0.0003])

  let cstr2 = c_formatParticleInfo (fromIntegral (8::Int)) 
                                   (fromIntegral (32::Int))
                                   lst1
                                   lst2
                                   lst3 
                                   (realToFrac (2.0::Double))
                                   (realToFrac (1.0::Double))
      str2 = unsafePerformIO $ peekCString cstr2
  putStrLn $ str2 -}

  