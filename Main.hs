module Main where

import DCPU
import DCPU.Types
import DCPU.Hardware.LEM1802

import Control.Monad.Trans.Maybe
import Control.Monad.Trans.State.Strict

import Data.Sequence
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Word

import Foreign

import Graphics.UI.Gtk

import System.IO

loadDCPUmem :: FilePath -> IO (Vector Word16)
loadDCPUmem path = do
  arr <- mallocArray 0x10000
  withFile path ReadMode $ \handle -> do
    h <- hGetBuf handle arr 0x10000
    V.generateM 0x10000 $ \n -> 
      if n <= h
        then peekElemOff arr n
        else return 0 

main :: IO ()
main = do
  [f] <- initGUI
  mem <- loadDCPUmem f
  lem <- fmap lem1802 newLEM1802
  let initDCPU = DCPU {
        _dcpuRam = (mem V.!) . fromIntegral,
        _dcpuRegisters = const 0,
        _dcpuIsInterruptQueueing = False,
        _dcpuInterruptQueue = empty,
        _dcpuHardware = singleton lem
        }
  runStateT (runMaybeT $ runStateT runDCPU initDCPU) 0
  return ()