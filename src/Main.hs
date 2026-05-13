-- haskell ray tracer

module Main where

import qualified Data.ByteString.Lazy as BL
import Data.ByteString.Builder
import Data.Maybe (mapMaybe, isJust, isNothing)
import Data.List (sortOn)
import System.Random (StdGen, mkStdGen, uniformR)

import Bmp (bmp, Color)

import Linear.V3
import Linear.Vector
import Linear.Metric

-- temporary color vector type
type V3Color = V3 Double
v3colorToColor :: V3Color -> Color
v3colorToColor v = (round r, round g, round b)
  where V3 r g b = v

data Sphere = Sphere { center :: V3 Double, radius :: Double }
data Ray = Ray { origin :: V3 Double, direction :: V3 Double }
-- t is the time (distance) taken for the ray to hit
-- p is the worldspace point of intersection
-- n is the normal of surface at p
data Hit = Hit { t :: Double, p :: V3 Double, n :: V3 Double }

-- given theta and phi, returns a unit vector pointing in that direction, where +y is up
sphericalToV3 :: Double -> Double -> V3 Double
sphericalToV3 theta phi = V3 (sin theta * cos phi) (cos theta) (sin theta * sin phi)

-- given a vector s in the +y hemisphere and normal n in worldspace,
-- return s such that +y is now n
-- s and n must be unit vectors
hemiToNormal :: V3 Double -> V3 Double -> V3 Double
hemiToNormal s n = sx*^t + sy*^n + sz*^b
  where V3 sx sy sz = s
        a = if abs(n `dot` V3 1 0 0) < 0.9 -- some arbitrary vector that is not n
            then V3 1 0 0
            else V3 0 1 0
        t = cross n a -- orthogonal to n
        b = cross n t -- orthogonal to n and t

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

-- calculates whether or not a ray intersects a sphere
-- https://en.wikipedia.org/wiki/Line%E2%80%93sphere_intersection#Calculation_using_vectors_in_3D
raySphere :: Ray -> Sphere -> Maybe Hit
raySphere (Ray o u) (Sphere c r) -- assuming u is normalized!
  | dis < 0 = Nothing
  | t1 > 0 = let pt = o + t1*^u in Just Hit { t=t1, p=pt, n=normalize $ pt - c }
  | t2 > 0 = let pt = o + t2*^u in Just Hit { t=t2, p=pt, n=normalize $ pt - c }
  | otherwise = Nothing
  where oc = o-c
        b = u `dot` oc
        dis = b^2 - (quadrance oc - r^2)
        t1 = -b - sqrt dis
        t2 = -b + sqrt dis
        pt t' = o + t'*^u

-- trace a Ray; return the closest intersection, if any
-- for now, only renders spheres
trace :: [Sphere] -> Ray -> Maybe Hit
trace spheres ray = case sortOn t (mapMaybe (raySphere ray) spheres) of
                      [] -> Nothing
                      (closest:_) -> Just closest

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
                                    pseudorandom = round $ abs $ cos (fromIntegral i*123123 + fromIntegral j)*fromIntegral i*1143
                                    gen = mkStdGen pseudorandom
                                    (_, v3color) = pathtrace gen spheres ray 400
                                in v3colorToColor v3color

  BL.writeFile "bmp.bmp" $ toLazyByteString $ bmp pathtraceColorFn (w, h) 
