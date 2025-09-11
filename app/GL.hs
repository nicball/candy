{-# LANGUAGE DerivingStrategies
           , DeriveAnyClass
           , FunctionalDependencies
           , TemplateHaskell
           , QuasiQuotes
           #-}

module GL
  ( GLException(..)
  , checkGLError
  , withGLFW
  , withWindow
  , withProgram
  , bindProgram
  , writeArrayBuffer
  , GLObject(..)
  , withObject
  , GLSlot(..)
  , withSlot
  , Texture
  , TextureSlot
  , texture2DSlot
  , Buffer
  , BufferSlot
  , arrayBufferSlot
  , VertexArray
  , VertexArraySlot
  , vertexArraySlot
  , Framebuffer
  , FramebufferSlot
  , framebufferSlot
  , Viewport(..)
  , ViewportSlot
  , viewportSlot
  , renderToTexture
  , Resolution(..)
  , pixelQuadToNDC
  , QuadRenderer
  , withQuadRenderer
  , drawQuadColor
  , drawQuadTexture
  ) where

import Control.Exception (Exception, throwIO, finally, assert)
import Control.Monad (when, unless, void)
import Data.ByteString qualified as BS
import Data.List.NonEmpty qualified as NonEmpty
import Data.String.Interpolate (__i)
import Data.Text qualified as Text
import Foreign.C qualified as C
import Foreign.Marshal qualified as C
import Foreign.Ptr qualified as C
import Foreign.Storable qualified as C
import Graphics.GL
import Graphics.UI.GLFW qualified as GLFW

import Config

data GLException = GLException Text.Text (NonEmpty.NonEmpty GLenum)
  deriving Show
  deriving anyclass Exception

newtype GLShaderCompilationError = GLShaderCompilationError BS.ByteString
  deriving Show
  deriving anyclass Exception

checkGLError :: Text.Text -> IO ()
checkGLError msg = do
  err <- getAllErrors []
  case err of
    [] -> pure ()
    e : es -> throwIO (GLException msg (e NonEmpty.:| es))
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
  GLFW.windowHint $ GLFW.WindowHint'TransparentFramebuffer True
  win <- GLFW.createWindow width height title Nothing Nothing
  case win of
    Nothing -> throwIO . GLFWException $ "Unable to create windows."
    Just w -> useWindow w `finally` GLFW.destroyWindow w
  where
    useWindow win = do
      GLFW.makeContextCurrent (Just win)
      action win

withShader :: GLenum -> BS.ByteString -> (GLuint -> IO a) -> IO a
withShader ty src action = do
  shader <- glCreateShader ty
  when (shader == 0) (checkGLError "withShader")
  useShader shader `finally` glDeleteShader shader
  where
    useShader shader = do
      BS.useAsCString src \pstr ->
        C.with pstr \ppstr ->
          glShaderSource shader 1 ppstr C.nullPtr
      glCompileShader shader
      status <- withPeek (glGetShaderiv shader GL_COMPILE_STATUS)
      when (status == GL_FALSE) do
        logLen <- withPeek (glGetShaderiv shader GL_INFO_LOG_LENGTH)
        C.withArray (replicate (fromIntegral logLen) 0) \errMsg -> do
          glGetShaderInfoLog shader logLen C.nullPtr errMsg
          throwIO . GLShaderCompilationError =<< BS.packCString (C.castPtr errMsg)
      action shader

withProgram :: Maybe BS.ByteString -> Maybe BS.ByteString -> Maybe BS.ByteString -> (GLuint -> IO a) -> IO a
withProgram vs gs fs action = do
  prog <- glCreateProgram
  when (prog == 0) (checkGLError "withProgram")
  useProgram prog `finally` glDeleteProgram prog
  where
    useProgram prog = do
      forCPS [(GL_VERTEX_SHADER, vs), (GL_GEOMETRY_SHADER, gs), (GL_FRAGMENT_SHADER, fs)]
             withShader'
             (mapM_ (attachShader prog))
      glLinkProgram prog
      status <- withPeek (glGetProgramiv prog GL_LINK_STATUS)
      when (status == GL_FALSE) do
        logLen <- withPeek (glGetProgramiv prog GL_INFO_LOG_LENGTH)
        C.withArray (replicate (fromIntegral logLen) 0) \errMsg -> do
          glGetProgramInfoLog prog logLen C.nullPtr errMsg
          throwIO . GLShaderCompilationError =<< BS.packCString (C.castPtr errMsg)
      action prog
    withShader' (_, Nothing) cont = cont Nothing
    withShader' (ty, Just src) cont = withShader ty src (cont . Just)
    attachShader _ Nothing = pure ()
    attachShader prog (Just shader) = glAttachShader prog shader

bindProgram :: GLuint -> IO a -> IO a
bindProgram prog action = do
  old <- fromIntegral <$> withPeek (glGetIntegerv GL_CURRENT_PROGRAM)
  glUseProgram prog
  action `finally` glUseProgram old

writeArrayBuffer :: [GLfloat] -> IO ()
writeArrayBuffer arr = do
  C.withArrayLen arr \size p -> do
    glBufferData GL_ARRAY_BUFFER (fromIntegral size * fromIntegral (C.sizeOf (0 :: GLfloat))) (C.castPtr p) GL_STREAM_DRAW

forCPS :: [a] -> (a -> (b -> r) -> r) -> ([b] -> r) -> r
forCPS [] _ k = k []
forCPS (x : xs) f k = f x \y -> forCPS xs f \ys -> k (y : ys)

withPeek :: C.Storable a =>(C.Ptr a -> IO b) -> IO a
withPeek action = C.alloca \p -> action p >> C.peek p

class GLObject a where
  genObject :: IO a
  deleteObject :: a -> IO ()

withObject :: GLObject a => (a -> IO b) -> IO b
withObject action = do
  obj <- genObject
  action obj `finally` deleteObject obj

class GLSlot a s | s -> a where
  getSlot :: s -> IO a
  setSlot :: s -> a -> IO ()

withSlot :: GLSlot a s => s -> a -> IO b -> IO b
withSlot s a action = do
  old <- getSlot s
  setSlot s a
  action `finally` setSlot s old

newtype Texture = Texture { id :: GLuint }

instance GLObject Texture where
  genObject = Texture <$> withPeek (glGenTextures 1)
  deleteObject o = C.with o.id (glDeleteTextures 1)

data TextureSlot = TextureSlot { target :: GLenum, parameter :: GLenum }

texture2DSlot :: TextureSlot
texture2DSlot = TextureSlot GL_TEXTURE_2D GL_TEXTURE_BINDING_2D

instance GLSlot Texture TextureSlot where
  getSlot s = Texture <$> withPeek (glGetIntegerv s.parameter . C.castPtr)
  setSlot s a = glBindTexture s.target a.id

newtype Buffer = Buffer { id :: GLuint }

instance GLObject Buffer where
  genObject = Buffer <$> withPeek (glGenBuffers 1)
  deleteObject o = C.with o.id (glDeleteBuffers 1)

data BufferSlot = BufferSlot { target :: GLenum, parameter :: GLenum }

arrayBufferSlot :: BufferSlot
arrayBufferSlot = BufferSlot GL_ARRAY_BUFFER GL_ARRAY_BUFFER_BINDING

instance GLSlot Buffer BufferSlot where
  getSlot s = Buffer <$> withPeek (glGetIntegerv s.parameter . C.castPtr)
  setSlot s a = glBindBuffer s.target a.id

newtype VertexArray = VertexArray { id :: GLuint }

instance GLObject VertexArray where
  genObject = VertexArray <$> withPeek (glGenVertexArrays 1)
  deleteObject o = C.with o.id (glDeleteVertexArrays 1)

data VertexArraySlot = VertexArraySlot

vertexArraySlot :: VertexArraySlot
vertexArraySlot = VertexArraySlot

instance GLSlot VertexArray VertexArraySlot where
  getSlot _ = VertexArray <$> withPeek (glGetIntegerv GL_VERTEX_ARRAY_BINDING . C.castPtr)
  setSlot _ o = glBindVertexArray o.id

newtype Framebuffer = Framebuffer { id :: GLuint }

instance GLObject Framebuffer where
  genObject = Framebuffer <$> withPeek (glGenFramebuffers 1)
  deleteObject o = C.with o.id (glDeleteFramebuffers 1)

data FramebufferSlot = FramebufferSlot

framebufferSlot :: FramebufferSlot
framebufferSlot = FramebufferSlot

instance GLSlot Framebuffer FramebufferSlot where
  getSlot _ = Framebuffer <$> withPeek (glGetIntegerv GL_FRAMEBUFFER_BINDING . C.castPtr)
  setSlot _ o = glBindFramebuffer GL_FRAMEBUFFER o.id

data Viewport = Viewport { x :: Int, y :: Int, width :: Int, height :: Int }

data ViewportSlot = ViewportSlot

viewportSlot :: ViewportSlot
viewportSlot = ViewportSlot

instance GLSlot Viewport ViewportSlot where
  getSlot _ = do
    [x, y, w, h] <- C.allocaArray 4 (\p -> glGetIntegerv GL_VIEWPORT p >> fmap fromIntegral <$> C.peekArray 4 p)
    pure $ Viewport x y w h
  setSlot _ (Viewport x y w h) = glViewport (fromIntegral x) (fromIntegral y) (fromIntegral w) (fromIntegral h)

renderToTexture :: Resolution -> IO a -> (Texture -> IO b) -> IO b
renderToTexture (Resolution w h) render action =
  withObject \texture -> do
    withSlot texture2DSlot texture do
      glTexImage2D GL_TEXTURE_2D 0 GL_RGBA (fromIntegral w) (fromIntegral h) 0 GL_RGBA GL_UNSIGNED_BYTE C.nullPtr
      checkGLError "glTexImage2D"
      glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER GL_NEAREST
      glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER GL_NEAREST
      glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_S GL_CLAMP_TO_BORDER
      glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_T GL_CLAMP_TO_BORDER
      C.withArray [0, 0, 0, 0] (glTexParameterfv GL_TEXTURE_2D GL_TEXTURE_BORDER_COLOR)
    withObject \fbo -> do
      withSlot framebufferSlot fbo do
        glFramebufferTexture2D GL_FRAMEBUFFER GL_COLOR_ATTACHMENT0 GL_TEXTURE_2D texture.id 0
        checkGLError "glFramebufferTexture2D"
        flip assert (pure ()) . (== GL_FRAMEBUFFER_COMPLETE) =<< glCheckFramebufferStatus GL_FRAMEBUFFER
        withSlot viewportSlot (Viewport 0 0 w h) (void render)
    withSlot texture2DSlot texture (glGenerateMipmap GL_TEXTURE_2D)
    action texture

data Resolution = Resolution { w :: Int, h :: Int }

pixelQuadToNDC :: Resolution -> ((Int, Int), (Int, Int), (Int, Int), (Int, Int)) -> [GLfloat]
pixelQuadToNDC res (tl, tr, bl, br) =
  concat
    [ scale tl 0 1
    , scale tr 1 1
    , scale bl 0 0
    , scale br 1 0
    ]
  where
    scale (x, y) dx dy =
      [ -1 + (fromIntegral x + dx)      * 2 / fromIntegral res.w
      ,  1 + (-fromIntegral y - 1 + dy) * 2 / fromIntegral res.h
      ]

data QuadRenderer = QuadRenderer
  { prog :: GLuint
  , vao :: GL.VertexArray
  , vbo :: GL.Buffer
  }

withQuadRenderer ::(QuadRenderer -> IO a) -> IO a
withQuadRenderer action =
  withProgram vs Nothing fs \prog -> do
    withObject \vao -> do
      withObject \vbo -> do
        withSlot arrayBufferSlot vbo do
          withSlot vertexArraySlot vao do
            glVertexAttribPointer 0 2 GL_FLOAT GL_FALSE 8 C.nullPtr
            glEnableVertexAttribArray 0
            glVertexAttribPointer 1 2 GL_FLOAT GL_FALSE 8 (C.plusPtr C.nullPtr 32)
            glEnableVertexAttribArray 1
        action QuadRenderer {..}
  where
    vs = Just
      [__i|
        \#version 330 core
        layout (location = 0) in vec2 v_pos;
        layout (location = 1) in vec2 v_tpos;
        out vec2 f_tpos;
        void main() {
          gl_Position = vec4(v_pos, 0, 1);
          f_tpos = v_tpos;
        }
      |]
    fs = Just
      [__i|
        \#version 330 core
        uniform vec4 f_color;
        uniform sampler2D tex;
        uniform bool use_texture;
        in vec2 f_tpos;
        out vec4 color;
        void main() {
          if (use_texture)
            color = texture(tex, f_tpos);
          else
            color = f_color;
        }
      |]

drawQuadColor :: QuadRenderer -> Resolution -> Color -> Int -> Int -> Int -> Int -> IO ()
drawQuadColor qr res color yStart yEnd xStart xEnd = do
  bindProgram qr.prog do
    withSlot vertexArraySlot qr.vao do
      withSlot arrayBufferSlot qr.vbo do
        writeArrayBuffer $
          pixelQuadToNDC res
            ( (xStart, yStart)
            , (xEnd, yStart)
            , (xStart, yEnd)
            , (xEnd, yEnd)
            )
          ++
          [ 0, 1, 1, 1, 0, 0, 1, 0 ]
        uColor <- C.withCAString "f_color" (glGetUniformLocation qr.prog)
        let Color{..} = color in
          glUniform4f uColor red green blue alpha
        glDrawArrays GL_TRIANGLE_STRIP 0 4

drawQuadTexture :: QuadRenderer -> Resolution -> Texture -> Int -> Int -> Int -> Int -> IO ()
drawQuadTexture qr res texture yStart yEnd xStart xEnd = do
  bindProgram qr.prog do
    withSlot vertexArraySlot qr.vao do
      withSlot arrayBufferSlot qr.vbo do
        writeArrayBuffer $
          pixelQuadToNDC res
            ( (xStart, yStart)
            , (xEnd, yStart)
            , (xStart, yEnd)
            , (xEnd, yEnd)
            )
          ++
          [ 0, 1, 1, 1, 0, 0, 1, 0 ]
        uUseTexture <- C.withCAString "use_texture" (glGetUniformLocation qr.prog)
        glUniform1i uUseTexture 1
        withSlot texture2DSlot texture do
          glDrawArrays GL_TRIANGLE_STRIP 0 4
