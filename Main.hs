module Main where
  
import System.Environment
import System.Process
import Safe
import Sierpinski
import Graphics.Rasterific

main :: IO ProcessHandle
main = do 
  args <- getArgs
  let buildDepth = read (args!!1) :: Int
      drawDepth = read (args!!2) :: Int
      coords = readDef [] (headDef [] args) :: [Float]
      tri = buildTriangle coords
        where buildTriangle [] = emptyTriangle
              buildTriangle [a, b, c, d, e, f] = Triangle (V2 a b) (V2 c d) (V2 e f)
              buildTriangle _ = emptyTriangle
      --expect a strung haskell list of doubles unsplit, return an emptyTriangle otherwise
  -- print $ "drawing: " ++ show tri
  drawSierpinskiTree [tri] buildDepth drawDepth 
  runCommand "open triangle.png"
