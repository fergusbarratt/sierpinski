module Sierpinski where 

import Codec.Picture( PixelRGBA8( .. ), writePng )
import Graphics.Rasterific
import Graphics.Rasterific.Texture

data Triangle = Triangle { topPoint :: Point
                        ,  rightPoint :: Point
                        ,  leftPoint :: Point
                      } deriving (Show, Eq)

type SierpinskiTree = [(Int, Triangle)]

emptyTriangle :: Triangle
emptyTriangle = Triangle (V2 0 0) (V2 0 0) (V2 0 0)

distance :: Point -> Point -> Float
distance (V2 x0 y0) (V2 x1 y1) = sqrt $ (x0+x1)**2 + (y0+y1)**2

midPoint :: Point -> Point -> Point
midPoint (V2 x0 y0) (V2 x1 y1) = V2 ((x1+x0)/2.0) ((y1+y0)/2.0) 

readTriangle :: Triangle -> [Point]
readTriangle (Triangle x y z) = [x, y, z]

drawTriangles :: [Triangle] -> IO ()
drawTriangles triangles = do
  let white = PixelRGBA8 255 255 255 255
      drawColor = PixelRGBA8 0 0x86 0xc1 255
      img = renderDrawing 400 400 white $
         withTexture (uniformTexture drawColor) $
              mapM_ (fill.polygon) [readTriangle triangle | triangle <- triangles]
  writePng "triangle.png" img

midTriangle :: Triangle -> Triangle
midTriangle (Triangle q p r) = Triangle (midPoint (topPoint (Triangle q p r)) (rightPoint (Triangle q p r))) (midPoint (rightPoint (Triangle q p r)) (leftPoint (Triangle q p r))) (midPoint (leftPoint (Triangle q p r)) (topPoint (Triangle q p r)))

invertTris :: [Triangle] -> [Triangle]
invertTris (tri:tris) = topTri : rightTri : leftTri : invertTris tris
  where topTri = Triangle outerTop innerTop innerLeft
        rightTri = Triangle outerRight innerRight innerTop
        leftTri = Triangle outerLeft innerLeft innerRight
        Triangle innerTop innerRight innerLeft = midTriangle tri
        Triangle outerTop outerRight outerLeft = tri
invertTris [] = []

initialSierpinski :: Triangle
initialSierpinski = Triangle (V2 200 0) (V2 400 400) (V2 0 400)

buildSierpinskiTree :: [Triangle] -> Int -> SierpinskiTree
buildSierpinskiTree tris 0 = zip (repeat 0) tris
buildSierpinskiTree tris height = zip (repeat height) (invertTris tris) ++ buildSierpinskiTree (invertTris tris) (height-1)

drawSierpinskiTree :: [Triangle] -> Int -> Int -> IO ()
drawSierpinskiTree initTri buildDepth drawDepth = drawTriangles triList
  where triList = map snd $ filter (\(depth, _) -> depth == drawDepth) $ buildSierpinskiTree initTri buildDepth 
