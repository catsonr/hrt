-- haskell ray tracer

module Main where

import qualified Data.ByteString.Lazy as BL
import Data.ByteString.Builder
import Data.Maybe (isJust, isNothing)
import System.Random (StdGen, mkStdGen, uniformR)

import Linear.V3
import Linear.Vector
import Linear.Metric

import Ray (Ray(..), Hit(..), sphericalToV3, hemiToNormal)
import Geometry (Sphere(..), trace)
import Bmp (bmp, Color)

-- temporary color vector type
type V3Color = V3 Double
v3colorToColor :: V3Color -> Color
v3colorToColor v = (round r, round g, round b)
  where V3 r g b = v

-- creates a Ray from the origin to the given "pixel" of the viewport
-- scans upside down since bmp is going to flip it rightside up
viewportRay :: Int -> Int -> Int -> Int -> Double -> Ray
viewportRay w h i j d = Ray {
  origin    = V3 0 0 0,
  direction = normalize $ V3 ((-r)/2 + dw/2 + i'*dw) (-0.5 + dh/2 + j'*dh) (-d)
}
  where w' = fromIntegral w
        h' = fromIntegral h
        i' = fromIntegral i
        j' = fromIntegral j
        r  = w' / h'
        dw = r  / w'
        dh = 1  / h'

pathtrace :: StdGen -> [Sphere] -> Ray -> Int -> (StdGen, V3Color)
pathtrace gen spheres ray depth
  | depth <= 0 = (gen, V3 0 0 0)
  | otherwise = case trace spheres ray of
    Nothing -> (gen, V3 120 190 255) -- sky color
    Just hit -> let (theta, gen1) = uniformR (0, pi/2) gen
                    (phi,   gen2) = uniformR (0, pi*2) gen1
                    bounce = hemiToNormal (sphericalToV3 theta phi) (n hit)
                    albedo = V3 0.8 0.1 0.1
                    newRay = Ray { origin = p hit + 0.001 *^ n hit, direction = bounce }
                    (gen3, color) = pathtrace gen2 spheres newRay (depth-1)
                in (gen3, color*albedo)

--- entrypoint
main = do
  let w = 800
  let h = 600

  let spheres = [ Sphere { center = V3 0 0 (-5), radius = 1.5 }
                , Sphere { center = V3 1.6 1.6 (-5), radius = 0.5 }
                , Sphere { center = V3 0 (-1001) (-5), radius = 1000 }
                ]

  let pathtraceColorFn (i, j) = let ray = viewportRay w h i j 1.0
                                    gen = mkStdGen (w*j + i)
                                    (_, v3color) = pathtrace gen spheres ray 4
                                in v3colorToColor v3color

  BL.writeFile "bmp.bmp" $ toLazyByteString $ bmp pathtraceColorFn (w, h) 
