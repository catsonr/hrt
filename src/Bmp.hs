module Bmp where

import Data.ByteString.Builder

-- Color represents a color with components R, G, and B. values above 255 are rounded down
type Color = (Int, Int, Int)

-- for a given width and height, return what would be the size of the bmp, in bytes
-- llm magic formula
bmpSize :: (Int, Int) -> Int
bmpSize (w, h) = 54 + rowSize*h
  where rowSize = ((w*3 + 3) `div` 4) * 4

-- for a given width, how many additional padding bytes are needed
paddingNeeded :: Int -> Int
paddingNeeded w = 
  if bytes `mod` 4 == 0 then 0
  else 4 - (bytes `mod` 4)
  where bytes = w*3

-- the actual byte padding to add to the end of each row
padding :: Int -> Builder
padding w = mconcat [ word8 0 | _ <- [1..paddingNeeded w] ]

-- the byte representation of a Color
pixel :: Color -> Builder
pixel (r, g, b) = word8 (fromIntegral b) <> word8 (fromIntegral g) <> word8 (fromIntegral r)

-- the bytes of the jth row of the bmp
row :: ((Int, Int) -> Color) -> Int -> Int -> Builder
row colorFn j w = mconcat [ pixel $ colorFn (i, j) | i <- [0..w-1] ] <> padding w

-- the bytes of all rows (entire image)
rows :: ((Int, Int) -> Color) -> Int -> Int -> Builder
rows colorFn w h = mconcat [ row colorFn j w | j <- [0..h-1] ]

-- generates a bmp with dimensions w, h as defined by colorFn
-- https://lmcnulty.me/words/bmp-output/
bmp :: ((Int, Int) -> Color) -> (Int, Int) -> Builder
bmp colorFn (w, h) = 
-- tag
  word8 0x42 <> word8 0x4d <>
-- header
  word32LE (fromIntegral $ bmpSize (w, h)) <> word32LE 0x00 <> word32LE 0x36 <>
-- DIB header
  word32LE 0x28 <> -- size of header
  word32LE (fromIntegral w) <> word32LE (fromIntegral h) <> -- w & h
  word16LE 0x01 <> -- number of color planes
  word16LE 0x18 <> -- bits per pixel
  word32LE 0x00 <> -- compression method
  word32LE 0x00 <> -- image size, ignored w no compression
  word32LE 0x2e23 <> -- horizontal resolution
  word32LE 0x2e23 <> -- vertical resolution
  word32LE 0x00 <> -- colors in pallete
  word32LE 0x00 <> -- important colors
  -- pixel data! (b, g, r, padding)
  rows colorFn w h

