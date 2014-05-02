module Main.Graphics ( initOpenGL
                     , resize
                     , updateGraphics
                     ) where

import Prelude ()
import BasicPrelude

import Wrappers.Events
import Wrappers.GLFW
import Wrappers.OpenGL hiding (position)

-- | Initialize the OpenGL context
initOpenGL :: IO ()
initOpenGL = do
        shadeModel $= Smooth
        clearDepth $= 1
        depthFunc $= Just Less
        hint PerspectiveCorrection $= Nicest

-- | Resize OpenGL view
resize :: Size -> IO ()
resize s@(Size w h) = do
        viewport $= (Position 0 0, s)
    
        matrixMode $= Projection
        loadIdentity
        perspective 45 (w // h) 0.1 64
    
        matrixMode $= Modelview 0
        loadIdentity
    where
        (//) = (/) `on` realToFrac

-- | One iteration of graphics
updateGraphics :: Window -> IO ()
updateGraphics wnd = drawFrame >> swapBuffers wnd

-- | Draw one frame of the game state
drawFrame :: IO ()
drawFrame = do
        -- Clear the screen
        clear [ ColorBuffer, DepthBuffer ]
        -- Reset the view
        loadIdentity

        -- draw here
        
        -- Write it all to the buffer
        flush
