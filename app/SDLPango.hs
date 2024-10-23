{-# LANGUAGE ForeignFunctionInterface, BlockArguments, DeriveAnyClass #-}

module SDLPango
  ( initialize

  , Context(..)
  , createContext
  , freeContext

  , Matrix(..)
  , matrixWhiteBack
  , matrixBlackBack
  , matrixTransparentBackBlackLetter
  , matrixTransparentBackWhiteLetter
  , matrixTransparentBackTransparentLetter

  , setDefaultColor
  , setMinimumSize
  , setDpi
  , setMarkup

  , getLayoutWidth
  , getLayoutHeight

  , draw
  , createSurfaceDraw
  ) where

import qualified Foreign.Ptr as C
import qualified Foreign.C.String as C
import qualified Foreign.C.Types as C
import qualified Data.ByteString as BS
import qualified SDL.Video.Renderer as SDL
import qualified SDL.Raw.Types as SDLRaw
import Control.Exception (Exception, throwIO, assert)
import Control.Monad (when)

foreign import ccall "SDLPango_Init"
  initialize :: IO ()

newtype Context = Context (C.Ptr Context_)
data Context_

foreign import ccall "SDLPango_CreateContext"
  createContext_ :: IO (C.Ptr Context_)

data SDLPangoContextCreationError = SDLPangoContextCreationError deriving (Show, Exception)
createContext :: IO Context
createContext = Context <$> do
  p <- createContext_
  when (p == C.nullPtr) (throwIO SDLPangoContextCreationError)
  pure p

foreign import ccall "SDLPango_FreeContext"
  freeContext :: Context -> IO ()

newtype Matrix = Matrix (C.Ptr Matrix_)
data Matrix_

foreign import ccall "&_MATRIX_WHITE_BACK"
  matrixWhiteBack :: Matrix

foreign import ccall "&_MATRIX_BLACK_BACK"
  matrixBlackBack :: Matrix

foreign import ccall "&_MATRIX_TRANSPARENT_BACK_BLACK_LETTER"
  matrixTransparentBackBlackLetter :: Matrix

foreign import ccall "&_MATRIX_TRANSPARENT_BACK_WHITE_LETTER"
  matrixTransparentBackWhiteLetter :: Matrix

foreign import ccall "&_MATRIX_TRANSPARENT_BACK_TRANSPARENT_LETTER"
  matrixTransparentBackTransparentLetter :: Matrix

foreign import ccall "SDLPango_SetDefaultColor"
  setDefaultColor :: Context -> Matrix -> IO ()

foreign import ccall "SDLPango_SetMinimumSize"
  setMinimumSize :: Context -> C.CInt -> C.CInt -> IO ()

foreign import ccall "SDLPango_SetDpi"
  setDpi :: Context -> C.CInt -> C.CInt -> IO ()

foreign import ccall "SDLPango_SetMarkup"
  setMarkup_ :: Context -> C.CString -> C.CInt -> IO ()

setMarkup :: Context -> BS.ByteString -> IO ()
setMarkup ctxt str = BS.useAsCString str \cstr -> setMarkup_ ctxt cstr (fromIntegral (BS.length str))

foreign import ccall "SDLPango_GetLayoutWidth"
  getLayoutWidth :: Context -> IO C.CInt

foreign import ccall "SDLPango_GetLayoutHeight"
  getLayoutHeight :: Context -> IO C.CInt

foreign import ccall "SDLPango_Draw"
  draw_ :: Context -> C.Ptr SDLRaw.Surface -> C.CInt -> C.CInt -> IO ()

draw :: Context -> SDL.Surface -> C.CInt -> C.CInt -> IO ()
draw ctxt (SDL.Surface raw _) = draw_ ctxt raw

foreign import ccall "SDLPango_CreateSurfaceDraw"
  createSurfaceDraw_ :: Context -> IO (C.Ptr SDLRaw.Surface)

createSurfaceDraw :: Context -> IO SDL.Surface
createSurfaceDraw = fmap (flip SDL.Surface Nothing . assertNotNull) . createSurfaceDraw_
  where assertNotNull p = assert (p /= C.nullPtr) p
