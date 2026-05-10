-- haskell ray tracer

module Main where

import qualified Data.ByteString.Lazy as BL
import Data.ByteString.Builder
import Bmp (bmp, Color)

import Linear.V3
import Linear.Metric
import Linear.Epsilon (nearZero)

data Sphere = Sphere { center :: V3 Double, radius :: Double }
data Ray = Ray { origin :: V3 Double, direction :: V3 Double }

-- determines the color of a given pixel based on its uv (origin top left)
colorAt :: (Int, Int) -> Color
colorAt _ = (0, 50, 255)

-- creates a Ray from the origin to the given "pixel" of the viewport
cast :: Int -> Int -> Int -> Int -> Double -> Ray
cast w h i j d = Ray {
  origin    = V3 0 0 0,
  direction = normalize $ V3 ((-r)/2 + dw/2 + i'*dw) (0.5 - dh/2 - j'*dh) (-d)
}
  where w' = fromIntegral w
        h' = fromIntegral h
        i' = fromIntegral i
        j' = fromIntegral j
        r  = w' / h'
        dw = r / w'
        dh = 1 / h'

raySphere :: Ray -> Sphere -> Maybe Double
raySphere (Ray o u) (Sphere c r) -- assuming u is normalized!
  | dis < 0 = Nothing
  | t1 > 0 = Just t1 -- two solutions, nearest intersection
  | t2 > 0 = Just t2 -- two solutions, farthest intersection (ray inside sphere)
  | otherwise = Nothing -- miss
  where oc = o-c
        b = u `dot` oc
        dis = b^2 - (quadrance oc - r^2)
        t1 = -b - sqrt dis
        t2 = -b + sqrt dis

main = do
  let w = 3
  let h = 3
  let ray = cast w h 1 1 1.0
  let sphere = Sphere { center = V3 0 0 (-5), radius = 3 }

  BL.writeFile "bmp.bmp" $ toLazyByteString $ bmp colorAt (w, h) 

  print $ raySphere ray sphere
