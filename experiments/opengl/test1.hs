import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT

main = do 
  (progname, _) <- getArgsAndInitialize
  createWindow "Hello World"
  displayCallback $= display
  mainLoop

vertex2 :: (GLfloat, GLfloat) -> IO ()
vertex2 (x, y) = vertex $ Vertex2 x y 

display = do 
  clear [ColorBuffer]
  renderPrimitive Quads $ do
    color $ (Color3 (1::GLfloat) 0 0)
    vertex2 (-0.5, 0.5)
    vertex2 (0.5, 0.5)
    vertex2 (0.5, -0.5)
    vertex2 (-0.5, -0.5)
  flush
