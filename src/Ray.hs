-- responsible for Ray and Hit

module Ray where

import Linear.V3
import Linear.Vector
import Linear.Metric

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
