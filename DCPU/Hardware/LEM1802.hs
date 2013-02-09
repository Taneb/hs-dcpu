{-# LANGUAGE TemplateHaskell #-}
module DCPU.Hardware.LEM1802 where

import DCPU.Types

import Control.Lens hiding (set)
import Control.Monad.Writer

import Data.Bits
import Data.IORef
import Data.Word

import Graphics.UI.Gtk

data LEM1802 = LEM1802 {
  _lem1802active :: Bool,
  _lem1802borderColour :: Word8,
  _lem1802video :: Word16,
  _lem1802font :: Word16,
  _lem1802palette :: Word16,
  lem1802refresh :: LEM1802 -> IO ()
  }

makeLenses ''LEM1802

lem1802 :: IORef LEM1802 -> Hardware
lem1802 lem = Hardware {
  hardwareID = 0x7349f615,
  hardwareVersion = 0x1802,
  hardwareManufacturer = 0x1c6c8b36,
  hardwareShoutbox = \n -> case n of
    0 -> do
      b <- use (dcpuLens $ Left B)
      liftIO $ modifyIORef lem (lem1802video .~ b)
    1 -> do
      b <- use (dcpuLens $ Left B)
      liftIO $ modifyIORef lem (lem1802font .~ b)
    2 -> do
      b <- use (dcpuLens $ Left B)
      liftIO $ modifyIORef lem (lem1802palette .~ b)
    3 -> do
      b <- use (dcpuLens $ Left B)
      liftIO $ modifyIORef lem (lem1802borderColour .~ fromIntegral (b .&. 0xf))
    4 -> undefined -- MEM_DUMP_FONT
    5 -> do
      tell (Sum 16)
      b <- use (dcpuLens $ Left B)
      sequence_ $ zipWith (assign . dcpuLens . Right) [b..] defaultPalette
    _ -> return ()
  }

newLEM1802 :: IO LEM1802
newLEM1802 = do
  window <- windowNewPopup
  pixbuf <- pixbufNew ColorspaceRgb False 8 128 96
  pixbufFill pixbuf 0 0 0 0
  image <- imageNewFromPixbuf pixbuf
  set window [
    windowTitle := "LEM1802",
    windowResizable := False,
    windowDefaultWidth := 148,
    windowDefaultHeight := 116,
    windowDestroyWithParent := True,
    containerChild := image
    ]
  undefined

defaultPalette :: [Word16]
defaultPalette = [
  0x0000,
  0x0a0a,
  0x00aa,
  0xa00a,
  0x0aa5,
  0x0aaa,
  0x5555,
  0x5f5f,
  0x55ff,
  0xf55f,
  0x5fff,
  0x5fff
  ]