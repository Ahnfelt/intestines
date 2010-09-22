module Main where


import Control.Concurrent
import Control.Monad
import Data.IORef
import Foreign 
import Graphics.UI.GLUT


initGL = do
    initialDisplayMode $= [ RGBMode
                          , WithDepthBuffer
                          , DoubleBuffered ]
    -- allocate two recources in GPU memory
    [vbo, cbo] <- genObjectNames 2

    -- fill 2xForeign.Array with vert/col data
    vertPtr <- newArray verts
    let vertSize = sizeOf (head verts) * length verts

    colPtr <- newArray colors
    let colSize = sizeOf (head colors) * length colors

    -- bind vertices to gfx mem
    bindBuffer ArrayBuffer $= Just vbo
    bufferData ArrayBuffer $= ( fromIntegral vertSize
                              , vertPtr
                              , StaticDraw)
    -- bind colors to gfx mem
    --bindBuffer ArrayBuffer $= Just cbo
    --bufferData ArrayBuffer $= ( fromIntegral colSize
    --                          , colPtr
    --                          , StaticDraw)
        {- i am a long comment -}
    -- idleCallback $= Just idle
    displayCallback $= display vertPtr


main = do
    -- initialize and make window
    getArgsAndInitialize
    initialWindowSize $= Size 800 600
    createWindow "gogo gadget pipeline!"
    -- set callbacks etc.
    clientState VertexArray $= Enabled
    state <- initGL
    -- main loop
    mainLoop

display vertPtr = do
    clear [ ColorBuffer
          , DepthBuffer ]
    withMappedBuffer ArrayBuffer ReadWrite (\p -> do
        arrayPointer VertexArray $= (VertexArrayDescriptor (fromIntegral $ length verts) Float 0 p)
        drawArrays Quads 0 (fromIntegral $ length verts)) undefined
    arrayPointer VertexArray $= (VertexArrayDescriptor (fromIntegral $ length verts) Float 0 vertPtr)
    drawArrays Quads 0 (fromIntegral $ length verts) 
    cube 0.1
    drawRangeElements Quads (0, fromIntegral $ length verts - 1) (fromIntegral $ length verts) Float vertPtr 
    flush
    swapBuffers

---------------------------------------
-- CONSTANTS
---------------------------------------
verts :: [ Vertex3 GLfloat ]
verts = [ Vertex3 (-0.5)   0.5   0.0
        , Vertex3   0.5    0.5   0.0
        , Vertex3   0.5  (-0.5)  0.0
        , Vertex3 (-0.5) (-0.5)  0.0 ]

colors :: [ Color3 GLfloat ]
colors = [ Color3 1.0 1.0 1.0
         , Color3 1.0 1.0 1.0
         , Color3 1.0 1.0 1.0 ]


cube :: GLfloat -> IO ()
cube w = do 
  renderPrimitive Quads $ do
    vertex $ Vertex3 w w w
    vertex $ Vertex3 w w (-w)
    vertex $ Vertex3 w (-w) (-w)
    vertex $ Vertex3 w (-w) w
    vertex $ Vertex3 w w w
    vertex $ Vertex3 w w (-w)
    vertex $ Vertex3 (-w) w (-w)
    vertex $ Vertex3 (-w) w w
    vertex $ Vertex3 w w w
    vertex $ Vertex3 w (-w) w
    vertex $ Vertex3 (-w) (-w) w
    vertex $ Vertex3 (-w) w w
    vertex $ Vertex3 (-w) w w
    vertex $ Vertex3 (-w) w (-w)
    vertex $ Vertex3 (-w) (-w) (-w)
    vertex $ Vertex3 (-w) (-w) w
    vertex $ Vertex3 w (-w) w
    vertex $ Vertex3 w (-w) (-w)
    vertex $ Vertex3 (-w) (-w) (-w)
    vertex $ Vertex3 (-w) (-w) w
    vertex $ Vertex3 w w (-w)
    vertex $ Vertex3 w (-w) (-w)
    vertex $ Vertex3 (-w) (-w) (-w)
    vertex $ Vertex3 (-w) w (-w)


