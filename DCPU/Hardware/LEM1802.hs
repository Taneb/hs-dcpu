module DCPU.Hardware.LEM1802 where

import DCPU.Types

import Control.Concurrent

import Data.Array.Unboxed
import Data.Array.IOArray
import Data.Function.Pointless
import Data.StateVar
import Data.Word

import Graphics.UI.WX

type LEM1802Font = UArray Word8 Word16
type LEM1802Palette = UArray Word8 Word16
type LEM1802VideoRam = UArray Word16 Word16

data LEM1802

{-
newLEM1802 :: IO LEM1802
newLEM1802 = do
  windowGeneratedCorrectly <- openWindow (Size 128 96) [] Window
  unless windowGeneratedCorrectly $ fail error "Could not create LEM1802 display."
  windowTitle $= "LEM1802 Display"
  -}

newLEM1802 :: IO LEM1802
newLEM1820 = do
  f <- frameFixed [text := "LEM1802"]
  l <- mkArray (0, 383) 0

getSlice :: Word16 -> Word16 -> UArray Word16 Word16 -> UArray Word16 Word16
getSlice size offset = ixmap (offset, offset + size) (subtract offset)

