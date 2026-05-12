-- haskell ray tracer

module Main where

import qualified Data.ByteString.Lazy as BL
import Data.ByteString.Builder
import Data.Maybe (mapMaybe)
import Data.List (sortOn)

import Bmp (bmp, Color)

import Linear.V3
import Linear.Vector
import Linear.Metric

data Sphere = Sphere { center :: V3 Double, radius :: Double }
data Ray = Ray { origin :: V3 Double, direction :: V3 Double }
-- t is the time (distance) taken for the ray to hit
-- p is the worldspace point of intersection
-- n is the normal of surface at p
data Hit = Hit { t :: Double, p :: V3 Double, n :: V3 Double }

-- given theta and phi, returns a (unit) vector pointing in that direction, where +y is up
sphericalToV3 :: Double -> Double -> V3 Double
sphericalToV3 theta phi = V3 (sin theta * cos phi) (cos theta) (sin theta * sin phi)

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

--- entrypoint
main = do
  let w = 300
  let h = 300
  let sphere = Sphere { center = V3 0 0 (-5), radius = 1.5 }
  -- global light direction, pointing towards the light source
  let lightDir :: V3 Double
      lightDir = normalize $ V3 1 1 1

  let spheres :: [Sphere]
      spheres = [Sphere { center = V3 0 0 (-5), radius = 1.5 }, Sphere { center = V3 1.6 1.6 (-5), radius = 0.5 } ]

  let multiSphereColorFn (i, j) = let ray = viewportRay w h i j 1.0
                                  in case trace spheres ray of
                                    Just hit -> (0, 255, 0)
                                    Nothing -> (0, 0, 0)

  let globalLightColorFn (i, j) = let ray = viewportRay w h i j 1.0
                                  in case raySphere ray sphere of
                                    Just hit -> let n' = n hit
                                                    r = round $ max 0 (n' `dot` lightDir)*255
                                                in (r, 0, 0)
                                    Nothing -> (0, 0, 0)

  let normalColorFn (i, j) = let ray = viewportRay w h i j 1.0
                       in case raySphere ray sphere of
                          Just hit -> let V3 nx ny nz = n hit
                                          color x = round $ ((x+1)/2)*255
                                      in (color nx, color ny, color nz)
                          Nothing -> (0, 0, 0)

  BL.writeFile "bmp.bmp" $ toLazyByteString $ bmp multiSphereColorFn (w, h) 
