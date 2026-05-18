-- responsible for geometric shapes and their interactions with Rays
-- for now, just spheres

module Geometry where

import Data.Maybe (mapMaybe)
import Data.List (sortOn)

import Linear.V3
import Linear.Vector
import Linear.Metric

import Ray (Ray(..), Hit(..))

data Sphere = Sphere { center :: V3 Double, radius :: Double }

-- calculates whether or not a ray intersects a sphere
-- https://en.wikipedia.org/wiki/Line%E2%80%93sphere_intersection#Calculation_using_vectors_in_3D
raySphere :: Ray -> Sphere -> Maybe Hit
raySphere (Ray o u) (Sphere c r) -- assuming u is normalized!
  | dis < 0 = Nothing
  | t1 > 0 = Just Hit { t=t1, p=pt t1, n=normalize $ pt t1 - c }
  | t2 > 0 = Just Hit { t=t2, p=pt t2, n=normalize $ pt t2 - c }
  | otherwise = Nothing
  where oc = o-c
        b = u `dot` oc
        dis = b^2 - (quadrance oc - r^2)
        t1 = -b - sqrt dis
        t2 = -b + sqrt dis
        pt t' = o + t'*^u

-- trace a Ray; return the closest intersection, if any
-- for now, only handles spheres
trace :: [Sphere] -> Ray -> Maybe Hit
trace spheres ray = case sortOn t (mapMaybe (raySphere ray) spheres) of
                      [] -> Nothing
                      (closest:_) -> Just closest
