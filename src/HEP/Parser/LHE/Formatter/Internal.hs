{-# LANGUAGE ForeignFunctionInterface #-}

module HEP.Parser.LHE.Formatter.Internal where 

import Foreign.C
import Foreign.Ptr

foreign import ccall "cformatter.h formatEventInfo" c_formatEventInfo 
  :: CInt -> CInt -> CDouble -> CDouble -> CDouble -> CDouble 
  -> CString

foreign import ccall "cformatter.h formatParticleInfo" c_formatParticleInfo 
  :: CInt -> CInt -> Ptr CInt -> Ptr CInt 
  -> Ptr CDouble -> CDouble -> CDouble -> CString


