{-# LANGUAGE DerivingStrategies
           , DeriveAnyClass
           , BlockArguments
           , PatternSynonyms
           #-}
module GL
  ( GLException(..)
  , checkGlError
  , withTexture2D
  ) where

import Graphics.GL
  ( pattern GL_NO_ERROR
  , pattern GL_TEXTURE_BINDING_2D
  , pattern GL_TEXTURE_2D
  , glGetError
  , GLenum
  , GLuint
  , glGetIntegerv
  , glBindTexture
  )
import qualified Data.List.NonEmpty as NonEmpty
import Control.Exception (Exception, throwIO, finally)
import qualified Foreign.Marshal.Utils as C
import qualified Foreign.Storable as C

newtype GLException = GLException (NonEmpty.NonEmpty GLenum)
  deriving Show
  deriving anyclass Exception

checkGlError :: IO ()
checkGlError = do
  err <- getAllErrors []
  case err of
    [] -> pure ()
    e : es -> throwIO (GLException (e NonEmpty.:| es))
  where
    getAllErrors es = do
      err <- glGetError
      if err == GL_NO_ERROR
        then pure es
        else getAllErrors (err : es)

withTexture2D :: GLuint -> IO a -> IO a
withTexture2D tex action = do
  old <- C.with 0 \p -> do
    glGetIntegerv GL_TEXTURE_BINDING_2D p
    fromIntegral <$> C.peek p
  glBindTexture GL_TEXTURE_2D tex
  action `finally` glBindTexture GL_TEXTURE_2D old
