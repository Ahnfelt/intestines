import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT

main = do
    (_, _) <- getArgsAndInitialize
    createWindow "Test"
    displayCallback $= display
    clearColor $= Color4 0.1 0.1 0.2 0.0
    clearDepth $= 1.0
    shadeModel $= Smooth
    depthFunc $= Just Lequal
    blend $= Enabled
    blendEquation $= FuncAdd
    blendFunc $= (SrcAlpha, OneMinusSrcAlpha)
    texture Texture2D $= Enabled
    textureFunction $= Replace
    mainLoop

vertex2 :: (GLfloat, GLfloat) -> IO ()
vertex2 (x, y) = vertex $ Vertex2 x y

color4 :: (GLfloat, GLfloat, GLfloat, GLfloat) -> IO ()
color4 (r, g, b, a) = color $ Color4 r g b a

display = do
    clear [ColorBuffer, DepthBuffer]
    loadIdentity
    renderPrimitive Quads $ do
        color4 (1.0, 0.0, 0.0, 0.5)
        vertex2 (-0.5, 0.5)
        color4 (0.0, 1.0, 0.0, 0.5)
        vertex2 (0.5, 0.5)
        color4 (0.0, 0.0, 1.0, 0.5)
        vertex2 (0.5, -0.5)
        color4 (1.0, 1.0, 0.0, 0.5)
        vertex2 (-0.5, -0.5)

        color4 (1.0, 0.0, 0.0, 0.5)
        vertex2 (-0.5 + 0.5, 0.5)
        color4 (0.0, 1.0, 0.0, 0.5)
        vertex2 (0.5 + 0.5, 0.5)
        color4 (0.0, 0.0, 1.0, 0.5)
        vertex2 (0.5 + 0.5, -0.5)
        color4 (1.0, 1.0, 0.0, 0.5)
        vertex2 (-0.5 + 0.5, -0.5)
    flush

