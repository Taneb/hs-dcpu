{-# LANGUAGE TemplateHaskell #-}
module DCPU.Hardware.LEM1802 where

import DCPU.Types

import Control.Arrow
import Control.Lens hiding (set)
import Control.Monad
import Control.Monad.State

import Data.Array.MArray
import Data.Bits
import Data.Bits.Lens
import Data.IORef
import Data.Tuple
import Data.Word

import Graphics.UI.Gtk

import System.Random

data LEM1802 = LEM1802 {
  _lem1802active :: Maybe Int,
  _lem1802borderColour :: Word8,
  _lem1802video :: Word16,
  _lem1802font :: Word16,
  _lem1802palette :: Word16,
  _lem1802blink :: Either Word8 Word8,
  lem1802refresh :: IORef LEM1802 -> DCPUM ()
  }

makeLenses ''LEM1802

lem1802 :: IORef LEM1802 -> Hardware
lem1802 lem = Hardware {
  hardwareID = 0x7349f615,
  hardwareVersion = 0x1802,
  hardwareManufacturer = 0x1c6c8b36,
  hardwareInterrupt = do
    a <- use $ dcpuLens (Left A)
    case a of
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
      4 -> do
        lift $ modify (+ 256)
        b <- use (dcpuLens $ Left B)
        zipWithM_ (assign . dcpuLens . Right) [b..] defaultFont
      5 -> do
        lift $ modify (+ 16)
        b <- use (dcpuLens $ Left B)
        zipWithM_ (assign . dcpuLens . Right) [b..] defaultPalette
      _ -> return (),
  hardwareRefresh = do
    f <- liftIO $ fmap lem1802refresh $ readIORef lem
    f lem
  }

newLEM1802 :: IO (IORef LEM1802)
newLEM1802 = do
  window <- windowNewPopup
  pixbuf <- pixbufNew ColorspaceRgb True 8 128 96
  pixbufFill pixbuf 0 0 0 0
  rowStride <- pixbufGetRowstride pixbuf
  image <- imageNewFromPixbuf pixbuf
  set window [
    windowTitle := "LEM1802",
    windowResizable := False,
    windowDefaultWidth := 148,
    windowDefaultHeight := 116,
    windowDestroyWithParent := True,
    containerChild := image
    ]
  widgetShowAll window
  activeTime <- fmap (length . filter id . take 200000 . randoms) newStdGen -- Bin(200000, 0.5), expectation = 1 second, variance = 0.5 seconds
  newIORef LEM1802 {
    _lem1802active = Just activeTime,
    _lem1802borderColour = 0x000,
    _lem1802video = 0x0000,
    _lem1802font = 0x0000,
    _lem1802palette = 0x0000,
    _lem1802blink = Right 500000,
    lem1802refresh = \lem -> do
      thisLem <- liftIO $ readIORef lem
      case _lem1802active &&& _lem1802video $ thisLem of
        (Just _, 0) -> return ()
        (Just _, _) -> liftIO $ modifyIORef lem (lem1802active . _Just -~ 1)
        (Nothing, 0) -> liftIO $ do
          newActiveTime <- fmap (length . filter id . take 200000 . randoms) newStdGen
          modifyIORef lem (lem1802active .~ Just newActiveTime >>> lem1802blink .~ Right 50000)
          pixbufFill pixbuf 0 0 0 0
          imageSetFromPixbuf image pixbuf
        (Nothing, video) -> do
          let modBlink = case _lem1802blink thisLem of
                Left 0 -> \_ -> Right 50000
                Right 0 -> \_ -> Left 50000
                Left _ -> left pred
                Right _ -> right pred
          liftIO $ modifyIORef lem (lem1802blink %~ modBlink)
          pixbufData <- liftIO $ pixbufGetPixels pixbuf
          font <- case _lem1802font thisLem of
            0 -> return defaultFont
            n -> forM [n .. n + 255] $ use . dcpuLens . Right
          palette <- case _lem1802palette thisLem of
            0 -> return defaultPalette
            n -> forM [n .. n + 15] $ use . dcpuLens . Right
          charsToRender <- forM [video .. video + 383] $ use . dcpuLens . Right
          let blinkStatus = has _Left (_lem1802blink thisLem)
              renderInfo = map (getRenderInfo blinkStatus) charsToRender
              fullRenderInfo = map (\d -> fillInRenderInfo d palette font) renderInfo
          liftIO $ zipWithM_ (uncurry . writeCharacter pixbufData rowStride) coords fullRenderInfo
    }

renderCharacter :: (Word16, Word16) -> (Word8, Word8) -> [(Word8, Word8, Word8)]
renderCharacter c (fg, bg) = let
  fgc = (0x11 * (fg .&. 0x0f00) `div` 0x100, 0x11 * (fg .&. 0x00f0) `div` 0x10, 0x11 * (fg .&. 0x000f))
  bgc = (0x11 * (bg .&. 0x0f00) `div` 0x100, 0x11 * (bg .&. 0x00f0) `div` 0x10, 0x11 * (bg .&. 0x000f))  
  in map (\x -> if x then fgc else bgc) (c ^.. both . backwards bits)

rgbToPixel :: (Word8, Word8, Word8) -> Word32
rgbToPixel (r,g,b) = 0x1000 * fromIntegral r + 0x100 * fromIntegral g + 0x10 * fromIntegral b

topLeftCoord :: Int -> (Int, Int) -> Int
topLeftCoord rowStride (x, y) = y * rowStride + x

coords :: [(Int, Int)]
coords = iterate (\(x,y) -> case x of
                     31 -> (0, y + 1)
                     _  -> (x + 1, y)
                     ) (0,0)

whereToEdit :: Int -> (Int, Int) -> [Int]
whereToEdit rowStride c = [topLeftCoord rowStride c .. topLeftCoord rowStride c + 3] >>= take 8 . iterate (+ rowStride)

writeCharacter :: PixbufData Int Word32 -> Int -> (Int, Int) -> (Word8, Word8) -> (Word16, Word16) -> IO ()
writeCharacter pixbufdat rowStride coord col char = zipWithM_ (writeArray pixbufdat) (whereToEdit rowStride coord) (map rgbToPixel $ renderCharacter char col)

getRenderInfo :: Bool -> Word16 -> (Word8, Word8)
getRenderInfo isBlinking f = let reallyBlinking = testBit f 7 in ((\(a, b) -> a * 0x10 + b) $ (if isBlinking /= reallyBlinking then swap else id)(fromIntegral $ (f .&. 0xf000) `div` 0x1000, fromIntegral $ (f .&. 0x0f00) `div` 0x0100), fromIntegral $ f .&. 0x7f)

fillInRenderInfo :: (Word8, Word8) -> [Word16] -> [Word16] -> ((Word8, Word8), (Word16,Word16))
fillInRenderInfo (colour, character) palette font = (splitWord16 $ palette !! fromIntegral colour, (font !! fromIntegral (character * 2), font !! fromIntegral (character * 2 + 1)))

splitWord16 :: Word16 -> (Word8, Word8)
splitWord16 n = (fromIntegral $ (n .&. 0xff00) `div` 0x100, fromIntegral $ n .&. 0x00ff)

defaultFont :: [Word16]
defaultFont = [0xb79e, 0x388e, 0x722c, 0x75f4, 0x19bb, 0x7f8f, 0x85f9, 0xb158, 0x242e, 0x2400, 0x082a, 0x0800, 0x0008, 0x0000, 0x0808, 0x0808,
               0x00ff, 0x0000, 0x00f8, 0x0808, 0x08f8, 0x0000, 0x080f, 0x0000, 0x000f, 0x0808, 0x00ff, 0x0808, 0x08f8, 0x0808, 0x08ff, 0x0000,
               0x080f, 0x0808, 0x08ff, 0x0808, 0x6633, 0x99cc, 0x9933, 0x66cc, 0xfef8, 0xe080, 0x7f1f, 0x0701, 0x0107, 0x1f7f, 0x80e0, 0xf8fe,
               0x5500, 0xaa00, 0x55aa, 0x55aa, 0xffaa, 0xff55, 0x0f0f, 0x0f0f, 0xf0f0, 0xf0f0, 0x0000, 0xffff, 0xffff, 0x0000, 0xffff, 0xffff,
               0x0000, 0x0000, 0x005f, 0x0000, 0x0300, 0x0300, 0x3e14, 0x3e00, 0x266b, 0x3200, 0x611c, 0x4300, 0x3629, 0x7650, 0x0002, 0x0100,
               0x1c22, 0x4100, 0x4122, 0x1c00, 0x1408, 0x1400, 0x081c, 0x0800, 0x4020, 0x0000, 0x0808, 0x0800, 0x0040, 0x0000, 0x601c, 0x0300,
               0x3e49, 0x3e00, 0x427f, 0x4000, 0x6259, 0x4600, 0x2249, 0x3600, 0x0f08, 0x7f00, 0x2745, 0x3900, 0x3e49, 0x3200, 0x6119, 0x0700,
               0x3649, 0x3600, 0x2649, 0x3e00, 0x0024, 0x0000, 0x4024, 0x0000, 0x0814, 0x2200, 0x1414, 0x1400, 0x2214, 0x0800, 0x0259, 0x0600,
               0x3e59, 0x5e00, 0x7e09, 0x7e00, 0x7f49, 0x3600, 0x3e41, 0x2200, 0x7f41, 0x3e00, 0x7f49, 0x4100, 0x7f09, 0x0100, 0x3e41, 0x7a00,
               0x7f08, 0x7f00, 0x417f, 0x4100, 0x2040, 0x3f00, 0x7f08, 0x7700, 0x7f40, 0x4000, 0x7f06, 0x7f00, 0x7f01, 0x7e00, 0x3e41, 0x3e00,
               0x7f09, 0x0600, 0x3e61, 0x7e00, 0x7f09, 0x7600, 0x2649, 0x3200, 0x017f, 0x0100, 0x3f40, 0x7f00, 0x1f60, 0x1f00, 0x7f30, 0x7f00,
               0x7708, 0x7700, 0x0778, 0x0700, 0x7149, 0x4700, 0x007f, 0x4100, 0x031c, 0x6000, 0x417f, 0x0000, 0x0201, 0x0200, 0x8080, 0x8000,
               0x0001, 0x0200, 0x2454, 0x7800, 0x7f44, 0x3800, 0x3844, 0x2800, 0x3844, 0x7f00, 0x3854, 0x5800, 0x087e, 0x0900, 0x4854, 0x3c00,
               0x7f04, 0x7800, 0x047d, 0x0000, 0x2040, 0x3d00, 0x7f10, 0x6c00, 0x017f, 0x0000, 0x7c18, 0x7c00, 0x7c04, 0x7800, 0x3844, 0x3800,
               0x7c14, 0x0800, 0x0814, 0x7c00, 0x7c04, 0x0800, 0x4854, 0x2400, 0x043e, 0x4400, 0x3c40, 0x7c00, 0x1c60, 0x1c00, 0x7c30, 0x7c00, 
               0x6c10, 0x6c00, 0x4c50, 0x3c00, 0x6454, 0x4c00, 0x0836, 0x4100, 0x0077, 0x0000, 0x4136, 0x0800, 0x0201, 0x0201, 0x0205, 0x0200]

defaultPalette :: [Word16]
defaultPalette = [0x0000,
                  0x000A,
                  0x00A0,
                  0x00AA,
                  0x0A00,
                  0x0A0A,
                  0x0A50,
                  0x0AAA,
                  0x0555,
                  0x055F,
                  0x05F5,
                  0x05FF,
                  0x0F55,
                  0x0F5F,
                  0x0FF5,
                  0x0FFF]