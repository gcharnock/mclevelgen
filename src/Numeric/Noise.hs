{-# LANGUAGE BangPatterns #-}
module Numeric.Noise where

import Data.Word
import Data.Vector.Unboxed ((!))
import qualified Data.Vector.Unboxed as Vector
import Data.Bits
import Data.Fixed

repeatLen :: Double
repeatLen = 2.0

permTab :: Vector.Vector Int 
permTab = Vector.fromList [151,160,137,91,90,15,  
    131,13,201,95,96,53,194,233,7,225,140,36,103,30,69,142,8,99,37,240,21,10,23,
    190, 6,148,247,120,234,75,0,26,197,62,94,252,219,203,117,35,11,32,57,177,33,
    88,237,149,56,87,174,20,125,136,171,168, 68,175,74,165,71,134,139,48,27,166,
    77,146,158,231,83,111,229,122,60,211,133,230,220,105,92,41,55,46,245,40,244,
    102,143,54, 65,25,63,161, 1,216,80,73,209,76,132,187,208, 89,18,169,200,196,
    135,130,116,188,159,86,164,100,109,198,173,186, 3,64,52,217,226,250,124,123,
    5,202,38,147,118,126,255,82,85,212,207,206,59,227,47,16,58,17,182,189,28,42,
    223,183,170,213,119,248,152, 2,44,154,163, 70,221,153,101,155,167, 43,172,9,
    129,22,39,253, 19,98,108,110,79,113,224,232,178,185, 112,104,218,246,97,228,
    251,34,242,193,238,210,144,12,191,179,162,241, 81,51,145,235,249,14,239,107,
    49,192,214, 31,181,199,106,157,184, 84,204,176,115,121,50,45,127, 4,150,254,
    138,236,205,93,222,114,67,29,24,72,243,141,128,195,78,66,215,61,156,180]

fade :: Double -> Double
fade !t = t * t * t * (t * (t * 6 - 15) + 10) -- 6t^5 - 15t^4 + 10t^3

inc :: Int -> Int
inc !num = let num' = fromIntegral (num + 1) in round $ num' `mod'` repeatLen

roundf :: Double -> Double
roundf = fromIntegral . round

floorf :: Double -> Double
floorf = fromIntegral . floor

lerp :: Double -> Double -> Double -> Double
lerp !a !b !x = a + x * (b - a)

grad :: Int -> Double -> Double -> Double -> Double
grad !hash !x !y !z = 
    case hash .&. 0xF of
        0x0 ->  x + y
        0x1 -> -x + y
        0x2 ->  x - y
        0x3 -> -x - y
        0x4 ->  x + z
        0x5 -> -x + z
        0x6 ->  x - z
        0x7 -> -x - z
        0x8 ->  y + z
        0x9 -> -y + z
        0xA ->  y - z
        0xB -> -y - z
        0xC ->  y + x
        0xD -> -y + z
        0xE ->  y - x
        0xF -> -y - z
        _ -> error "hash out of range"

perlin :: Double -> Double -> Double -> Double
perlin !x !y !z = let
    x' = x `mod'` 2.0
    y' = y `mod'` 2.0
    z' = z `mod'` 2.0

    -- Find coordinates of unit cube
    xi = (floor x' :: Int) .&. 255
    yi = (floor y' :: Int) .&. 255 
    zi = (floor z' :: Int) .&. 255

    -- Find the coordinates inside the cube
    xf = x' - floorf x' :: Double  
    yf = y' - floorf y' :: Double
    zf = z' - floorf z' :: Double

    u = fade xf
    v = fade yf
    w = fade zf

    p = (permTab !)

    -- generate a random value for each corner of the cube
    aaa = p $ p (p (    xi ) +     yi ) +     zi 
    aba = p $ p (p (    xi ) + inc(yi)) +     zi 
    aab = p $ p (p (    xi ) +     yi ) + inc(zi)
    abb = p $ p (p (    xi ) + inc(yi)) + inc(zi)
    baa = p $ p (p (inc(xi)) +     yi ) +     zi 
    bba = p $ p (p (inc(xi)) + inc(yi)) +     zi 
    bab = p $ p (p (inc(xi)) +     yi ) + inc(zi)
    bbb = p $ p (p (inc(xi)) + inc(yi)) + inc(zi)
    
    -- The gradient function calculates the dot product between a pseudorandom                      
    -- gradient vector and the vector from the input coordinate to the 8
    -- surrounding points in its unit cube.
    -- This is all then lerped together as a sort of weighted average based on the faded (u,v,w)
    -- values we made earlier.

    x1 = lerp (grad aaa xf  yf    zf) (grad baa (xf-1)  yf    zf) u
    x2 = lerp (grad aba xf (yf-1) zf) (grad bba (xf-1) (yf-1) zf) u
    y1 = lerp x1 x2 v

    x3 = lerp (grad aab xf  yf    (zf-1)) (grad bab (xf-1)  yf    (zf-1)) u
    x4 = lerp (grad abb xf (yf-1) (zf-1)) (grad bbb (xf-1) (yf-1) (zf-1)) u
    y2 = lerp x1 x2 v
    in ((lerp y1 y2 w) + 1) / 2;
