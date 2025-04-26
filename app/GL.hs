{-# LANGUAGE DerivingStrategies
           , DeriveAnyClass
           , BlockArguments
           , PatternSynonyms
           #-}

module GL
  ( GLException(..)
  , checkGlError
  , withGLFW
  , withWindow
  , withTexture2D
  , withProgram
  , withArrayBuffer
  , bindArrayBuffer
  , writeArrayBuffer
  , withVAO
  , bindVAO
  ) where

import Graphics.GL
  ( pattern GL_NO_ERROR
  , pattern GL_TEXTURE_BINDING_2D
  , pattern GL_TEXTURE_2D
  , pattern GL_FALSE
  , pattern GL_COMPILE_STATUS
  , pattern GL_INFO_LOG_LENGTH
  , pattern GL_VERTEX_SHADER
  , pattern GL_GEOMETRY_SHADER
  , pattern GL_FRAGMENT_SHADER
  , pattern GL_LINK_STATUS
  , pattern GL_CURRENT_PROGRAM
  , pattern GL_ARRAY_BUFFER
  , pattern GL_ARRAY_BUFFER_BINDING
  , pattern GL_STREAM_DRAW
  , glGetError
  , GLenum
  , GLuint
  , GLfloat
  , glGetIntegerv
  , glBindTexture
  , glCreateShader
  , glDeleteShader
  , glCompileShader
  , glShaderSource
  , glGetShaderiv
  , glGetShaderInfoLog
  , glCreateProgram
  , glDeleteProgram
  , glAttachShader
  , glLinkProgram
  , glGetProgramiv
  , glGetProgramInfoLog
  , glUseProgram
  , glGenBuffers
  , glDeleteBuffers
  , glBindBuffer
  , glBufferData
  , glGenVertexArrays
  , glDeleteVertexArrays
  , glBindVertexArray
  )
import qualified Graphics.UI.GLFW as GLFW
import qualified Data.List.NonEmpty as NonEmpty
import Control.Exception (Exception, throwIO, finally)
import qualified Foreign.Marshal.Utils as C
import qualified Foreign.Marshal.Array as C
import qualified Foreign.Storable as C
import qualified Foreign.Ptr as C
import qualified Data.ByteString as BS
import Control.Monad (when, unless)

newtype GLException = GLException (NonEmpty.NonEmpty GLenum)
  deriving Show
  deriving anyclass Exception

newtype GLShaderCompilationError = GLShaderCompilationError BS.ByteString
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

newtype GLFWException = GLFWException String
  deriving Show
  deriving anyclass Exception

withGLFW :: IO a -> IO a
withGLFW action = do
  GLFW.setErrorCallback $ Just \err str ->
      throwIO . GLFWException $ show err ++ ": " ++ str
  success <- GLFW.init
  unless success . throwIO . GLFWException $ "Unable to initialize GLFW."
  action `finally` GLFW.terminate

withWindow :: Int -> Int -> String -> (GLFW.Window -> IO a) -> IO a
withWindow width height title action = do
  GLFW.windowHint $ GLFW.WindowHint'ContextVersionMajor 3
  GLFW.windowHint $ GLFW.WindowHint'ContextVersionMinor 3
  GLFW.windowHint $ GLFW.WindowHint'OpenGLForwardCompat True
  GLFW.windowHint $ GLFW.WindowHint'OpenGLProfile GLFW.OpenGLProfile'Core
  win <- GLFW.createWindow width height title Nothing Nothing
  case win of
    Nothing -> throwIO . GLFWException $ "Unable to create windows."
    Just w -> useWindow w `finally` GLFW.destroyWindow w
  where
    useWindow win = do
      GLFW.makeContextCurrent (Just win)
      action win

withTexture2D :: GLuint -> IO a -> IO a
withTexture2D tex action = do
  old <- fromIntegral <$> withPeek 0 (glGetIntegerv GL_TEXTURE_BINDING_2D)
  glBindTexture GL_TEXTURE_2D tex
  action `finally` glBindTexture GL_TEXTURE_2D old

withShader :: GLenum -> BS.ByteString -> (GLuint -> IO a) -> IO a
withShader ty src action = do
  shader <- glCreateShader ty
  when (shader == 0) checkGlError
  useShader shader `finally` glDeleteShader shader
  where
    useShader shader = do
      BS.useAsCString src \pstr ->
        C.with pstr \ppstr ->
          glShaderSource shader 1 ppstr C.nullPtr
      glCompileShader shader
      status <- withPeek 0 (glGetShaderiv shader GL_COMPILE_STATUS)
      when (status == GL_FALSE) do
        logLen <- withPeek 0 (glGetShaderiv shader GL_INFO_LOG_LENGTH)
        C.withArray (replicate (fromIntegral logLen) 0) \errMsg -> do
          glGetShaderInfoLog shader logLen C.nullPtr errMsg
          throwIO . GLShaderCompilationError =<< BS.packCString (C.castPtr errMsg)
      action shader

withProgram :: BS.ByteString -> BS.ByteString -> BS.ByteString -> (GLuint -> IO a) -> IO a
withProgram vs gs fs action = do
  prog <- glCreateProgram
  when (prog == 0) checkGlError
  useProgram prog `finally` glDeleteProgram prog
  where
    useProgram prog = do
      forCPS [(GL_VERTEX_SHADER, vs), (GL_GEOMETRY_SHADER, gs), (GL_FRAGMENT_SHADER, fs)]
             (uncurry withShader)
             (mapM_ (glAttachShader prog))
      glLinkProgram prog
      status <- withPeek 0 (glGetProgramiv prog GL_LINK_STATUS)
      when (status == GL_FALSE) do
        logLen <- withPeek 0 (glGetProgramiv prog GL_INFO_LOG_LENGTH)
        C.withArray (replicate (fromIntegral logLen) 0) \errMsg -> do
          glGetProgramInfoLog prog logLen C.nullPtr errMsg
          throwIO . GLShaderCompilationError =<< BS.packCString (C.castPtr errMsg)
      old <- fromIntegral <$> withPeek 0 (glGetIntegerv GL_CURRENT_PROGRAM)
      glUseProgram prog
      action prog `finally` glUseProgram old

withArrayBuffer :: (GLuint -> IO a) -> IO a
withArrayBuffer action = do
  buf <- withPeek 0 (glGenBuffers 1)
  action buf `finally` C.with buf (glDeleteBuffers 1)

bindArrayBuffer :: GLuint -> IO a -> IO a
bindArrayBuffer buf action = do
  old <- withPeek 0 (glGetIntegerv GL_ARRAY_BUFFER_BINDING . C.castPtr)
  glBindBuffer GL_ARRAY_BUFFER buf
  action `finally` glBindBuffer GL_ARRAY_BUFFER old

writeArrayBuffer :: [GLfloat] -> IO ()
writeArrayBuffer arr = do
  C.withArrayLen arr \size p -> do
    glBufferData GL_ARRAY_BUFFER (fromIntegral size * fromIntegral (C.sizeOf (0 :: GLfloat))) (C.castPtr p) GL_STREAM_DRAW

withVAO :: (GLuint -> IO a) -> IO a
withVAO action = do
  vao <- withPeek 0 (glGenVertexArrays 1)
  action vao `finally` C.with vao (glDeleteVertexArrays 1)

bindVAO :: GLuint -> IO a -> IO a
bindVAO vao action = do
  glBindVertexArray vao
  action `finally` glBindVertexArray 0

forCPS :: [a] -> (a -> (b -> r) -> r) -> ([b] -> r) -> r
forCPS [] _ k = k []
forCPS (x : xs) f k = f x \y -> forCPS xs f \ys -> k (y : ys)

withPeek :: C.Storable a => a -> (C.Ptr a -> IO ()) -> IO a
withPeek val action = C.with val \p -> action p >> C.peek p
