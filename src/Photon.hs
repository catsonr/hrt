-- responsible for the simulation of photons

module Photon where

import Linear.V3
import Linear.Vector

import Ray (Ray(..), Hit(..), sphericalToV3, hemiToNormal)
import Geometry (Sphere(..), trace)

data Photon = Photon { pOrigin :: V3 Double, pDirection :: V3 Double, pPower :: V3 Double }

tracePhoton :: [Sphere] -> Photon -> Int -> [Photon]
tracePhoton spheres photon depth
  | depth <= 0 = []
  | otherwise = case trace spheres ray of
    Nothing -> []
    Just hit -> 
      let albedo = V3 0.8 0.1 0.1
          stored = Photon { pOrigin = p hit
                          , pDirection = pDirection photon
                          , pPower = pPower photon
                          }
          theta = 0.1
          phi = 0.1
          bounce = Photon { pOrigin = p hit + 0.001 *^ n hit
                          , pDirection = hemiToNormal (sphericalToV3 theta phi) (n hit)
                          , pPower = pPower photon * albedo
                          }
          rest = tracePhoton spheres bounce (depth-1)
      in stored:rest
    where ray = Ray { origin = pOrigin photon, direction = pDirection photon }

-- traces all photons and concatinates the results into one list (the photon map!)
generatePhotonMap :: [Sphere] -> [Photon] -> [Photon]
generatePhotonMap spheres = concatMap (\photon -> tracePhoton spheres photon 4)

data PointLight = PointLight { plOrigin :: V3 Double, plIntensity :: Double }

-- for a given pointlight pl and sphere subdivision counts w, h, returns the list of
-- photons to be traced
pointLightPhotons :: PointLight -> Int -> Int -> [Photon]
pointLightPhotons pl w h =
  [ Photon { pOrigin = plOrigin pl,
             pDirection = sphericalToV3 theta phi,
             pPower = power } |
    let power = V3 1 1 1 ^* (plIntensity pl / fromIntegral (w*h)),
    i <- [0..w-1],
    let theta = pi * fromIntegral i / fromIntegral w,
    j <- [0..h-1],
    let phi = 2*pi * fromIntegral j / fromIntegral h
  ]
