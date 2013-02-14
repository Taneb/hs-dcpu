{-# LANGUAGE TupleSections #-}
module DCPU where

import DCPU.Types

import Control.Applicative

import Control.Exception
import Control.Lens hiding ((<|), op)
import Control.Monad.State.Strict hiding (mapM_)

import Data.Bits
import Data.Foldable hiding (elem)
import Data.Int
import Data.Monoid
import Data.Sequence
import Data.Word

import System.CPUTime

import Prelude hiding (length, mapM_)

memPartA :: Word16 -> DCPUM Word16
memPartA n | n <= 0x07 = use . dcpuLens . Left . toEnum $ fromIntegral n
           | n <= 0x0f = do
             addr <- use . dcpuLens . Left . toEnum $ fromIntegral (n - 8)
             use . dcpuLens $ Right addr
           | n <= 0x17 = do
             offset <- memPartA 0x1f
             addr <- use . dcpuLens . Left . toEnum $ fromIntegral (n - 16)
             use . dcpuLens $ Right (addr + offset)
           | n == 0x18 = do
             sp <- use . dcpuLens $ Left SP
             dcpuLens (Left SP) += 1
             use . dcpuLens $ Right sp
           | n == 0x19 = do
             sp <- use . dcpuLens $ Left SP
             use . dcpuLens $ Right sp
           | n == 0x1a = do
             offset <- memPartA 0x1f
             sp <- use . dcpuLens $ Left SP
             use . dcpuLens $ Right (sp + offset)
           | n == 0x1b = use . dcpuLens $ Left SP
           | n == 0x1c = use . dcpuLens $ Left PC
           | n == 0x1d = use . dcpuLens $ Left EX
           | n == 0x1e = do
             addr <- memPartA 0x1f
             use . dcpuLens $ Right addr
           | n == 0x1f = do
             lift $ modify (+1)
             pc <- use . dcpuLens $ Left PC
             dcpuLens (Left PC) += 1
             use . dcpuLens $ Right pc
           | otherwise = return (n - 33)
                         
memPartB :: Word16 -> DCPUM (Word16, Either RegCode Word16)                         
memPartB n | n <= 0x07 = let rc = Left . toEnum $ fromIntegral n in (, rc) <$> use (dcpuLens rc)
           | n <= 0x0f = do
             addr <- use . dcpuLens . Left . toEnum $ fromIntegral (n - 8)
             uses (dcpuLens $ Right addr) (, Right addr)
           | n <= 0x17 = do
             offset <- memPartA 0x1f
             addr <- use . dcpuLens . Left . toEnum $ fromIntegral (n - 16)
             uses (dcpuLens . Right $ addr + offset) (, Right (addr + offset))
           | n == 0x18 = do
             dcpuLens (Left SP) -= 1
             sp <- use . dcpuLens $ Left SP
             uses (dcpuLens $ Right sp) (, Right sp)
           | n == 0x19 = do
             sp <- use . dcpuLens $ Left SP
             uses (dcpuLens $ Right sp) (, Right sp)
           | n == 0x1a = do
             offset <- memPartA 0x1f
             sp <- use . dcpuLens $ Left SP
             uses (dcpuLens $ Right (sp + offset)) (, Right (sp + offset))
           | n == 0x1b = (, Left SP) <$> use (dcpuLens $ Left SP)
           | n == 0x1c = (, Left PC) <$> use (dcpuLens $ Left PC)
           | n == 0x1d = (, Left EX) <$> use (dcpuLens $ Left EX)
           | n == 0x1e = do
             addr <- memPartA 0x1f
             uses (dcpuLens $ Right addr) (, Right addr)
           | n == 0x1f = do
             pc <- memPartA 0x1f
             uses (dcpuLens $ Right pc) (, Right pc)
           | otherwise = return (n - 33, Left N)
                         
sizeOfInstruction :: DCPUM Word16
sizeOfInstruction = do
  pc <- use . dcpuLens $ Left PC
  op <- use . dcpuLens $ Right pc
  let f = (`elem` ([0x10..0x17] ++ [0x1a, 0x1e, 0x1f]))
      o = op .&. 0x1F
      a = f $ (op `shiftR` 5) .&. 0x1F
      b = f $ op `shiftR` 10
  return (1 + (if a then 1 else 0) + (if b && o /= 0 then 1 else 0))
  
interrupt :: Word16 -> DCPUM ()
interrupt msg = do
  q <- use dcpuIsInterruptQueueing
  if q
    then dcpuInterruptQueue %= (msg <|)
    else do
      ia <- use . dcpuLens $ Left IA
      unless (ia == 0) $ do
        dcpuIsInterruptQueueing .= True
        pc <- use . dcpuLens $ Left PC
        a <- use . dcpuLens $ Left A
        dcpuLens (Left SP) -= 1
        sp1 <- use . dcpuLens $ Left SP
        dcpuLens (Right sp1) .= pc
        dcpuLens (Left SP) -= 1
        sp2 <- use . dcpuLens $ Left SP
        dcpuLens (Right sp2) .= a
        dcpuLens (Left PC) .= ia
        dcpuLens (Left A) .= msg
  
step :: DCPUM ()
step = do
  timeToPause <- lift get
  ago <- liftIO getCPUTime
  let loop = do
        now <- liftIO getCPUTime   
        if ago + timeToPause * 10 ^ 9 > now
          then loop
          else return ()
  loop
  lift $ put 0
  pc <- use . dcpuLens $ Left PC
  dcpuLens (Left PC) += 1
  op <- use . dcpuLens $ Right pc
  let opcode = op .&. 0x1F
      bcode = (op `shiftR` 5) .&. 0x1F
      acode = op `shiftR` 10
  case opcode of
    0x00 -> case bcode of
      0x00 -> liftIO $ throwIO DCPUUndefinedOperation -- reserved for future expansion
      0x01 -> do
        lift $ modify (+ 3)
        a <- memPartA acode
        pc1 <- use . dcpuLens $ Left PC
        dcpuLens (Left SP) -= 1
        sp <- use . dcpuLens $ Left SP
        dcpuLens (Right sp) .= pc1
        dcpuLens (Left PC) .= a
      0x02 -> liftIO $ throwIO DCPUUndefinedOperation
      0x03 -> liftIO $ throwIO DCPUUndefinedOperation
      0x04 -> liftIO $ throwIO DCPUUndefinedOperation
      0x05 -> liftIO $ throwIO DCPUUndefinedOperation
      0x06 -> liftIO $ throwIO DCPUUndefinedOperation
      0x07 -> liftIO $ throwIO DCPUUndefinedOperation
      0x08 -> do
        lift $ modify (+ 4)
        a <- memPartA acode
        interrupt a
      0x09 -> do
        lift $ modify (+ 1)
        (_, addr) <- memPartB acode
        ia <- use . dcpuLens $ Left IA
        dcpuLens addr .= ia
      0x0a -> do
        lift $ modify (+ 1)
        a <- memPartA acode
        dcpuLens (Left IA) .= a
      0x0b -> do
        lift $ modify (+ 1)
        dcpuIsInterruptQueueing .= False
        sp1 <- use . dcpuLens $ Left SP
        dcpuLens (Left SP) += 1
        a <- use . dcpuLens $ Right sp1
        dcpuLens (Left A) .= a
        sp2 <- use . dcpuLens $ Left SP
        dcpuLens (Left SP) += 1
        pc1 <- use . dcpuLens $ Right sp2
        dcpuLens (Left PC) .= pc1
      0x0c -> do
        lift $ modify (+ 2)
        a <- memPartA acode
        dcpuIsInterruptQueueing .= (a /= 0)
      0x0d -> liftIO $ throwIO DCPUUndefinedOperation
      0x0e -> liftIO $ throwIO DCPUUndefinedOperation
      0x0f -> liftIO $ throwIO DCPUUndefinedOperation
      0x10 -> do
        lift $ modify (+ 2)
        noOfConnectedDevices <- uses dcpuHardware $ fromIntegral . length 
        dcpuLens (Left A) .= noOfConnectedDevices
      0x11 -> do
        lift $ modify (+ 4)
        a <- memPartA acode
        thisDevice <- preuse $ dcpuHardware . ix (fromIntegral a)
        case thisDevice of
          Nothing -> do
            dcpuLens (Left A) .= 0
            dcpuLens (Left B) .= 0
            dcpuLens (Left C) .= 0
            dcpuLens (Left X) .= 0
            dcpuLens (Left Y) .= 0
          Just device -> do
            dcpuLens (Left B) .= fromIntegral (hardwareID device `shiftR` 16)
            dcpuLens (Left A) .= fromIntegral (hardwareID device)
            dcpuLens (Left C) .= hardwareVersion device
            dcpuLens (Left Y) .= fromIntegral (hardwareManufacturer device `shiftR` 16)
            dcpuLens (Left X) .= fromIntegral (hardwareManufacturer device)
      0x12 -> do
        lift $ modify (+ 4)
        a <- memPartA acode
        thisDevice <- preuse $ dcpuHardware . ix (fromIntegral a)
        case thisDevice of
          Nothing -> return ()
          Just device -> hardwareInterrupt device
      _ -> liftIO $ throwIO DCPUUndefinedOperation
    0x01 -> do
      lift $ modify (+ 1)
      a <- memPartA acode
      (_, addr) <- memPartB bcode
      dcpuLens addr .= a
    0x02 -> do
      lift $ modify (+ 2)
      a <- memPartA acode
      (b, addr) <- memPartB bcode
      dcpuLens addr .= a + b
      dcpuLens (Left EX) .= if (a + b) < a then 1 else 0
    0x03 -> do
      lift $ modify (+ 2)
      a <- memPartA acode
      (b, addr) <- memPartB bcode
      dcpuLens addr .= b - a
      dcpuLens (Left EX) .= if a > b then -1 else 0
    0x04 -> do
      lift $ modify (+ 2)
      a <- memPartA acode
      (b, addr) <- memPartB bcode
      dcpuLens addr .= b * a
      let a', b' :: Word32
          a' = fromIntegral a
          b' = fromIntegral b
      dcpuLens (Left EX) .= fromIntegral ((a' * b') `shiftR` 16)
    0x05 -> do
      lift $ modify (+ 2)
      a <- fromIntegral <$> memPartA acode :: DCPUM Int16
      (b, addr) <- over _1 fromIntegral <$> memPartB bcode :: DCPUM (Int16, Either RegCode Word16)
      dcpuLens addr .= fromIntegral (b * a)
      let a', b' :: Int32
          a' = fromIntegral a
          b' = fromIntegral b
      dcpuLens (Left EX) .= fromIntegral ((a' * b') `shiftR` 16)
    0x06 -> do
      lift $ modify (+ 3)
      a <- memPartA acode
      (b, addr) <- memPartB bcode
      case a of
        0 -> do
          dcpuLens addr .= 0
          dcpuLens (Left EX) .= 0
        _ -> do
          dcpuLens addr .= b `div` a
          let a', b' :: Word32
              a' = fromIntegral a
              b' = fromIntegral b
          dcpuLens (Left EX) .= fromIntegral (b' `shiftL` 16 `div` a')
    0x07 -> do
      lift $ modify (+ 3)
      a <- fromIntegral <$> memPartA acode :: DCPUM Int16
      (b, addr) <- over _1 fromIntegral <$> memPartB bcode :: DCPUM (Int16, Either RegCode Word16)
      case a of
        0 -> do
          dcpuLens addr .= 0
          dcpuLens (Left EX) .= 0
        _ -> do
          dcpuLens addr .= fromIntegral (b `div` a)
          let a', b' :: Int32
              a' = fromIntegral a
              b' = fromIntegral b
          dcpuLens (Left EX) .= fromIntegral (b' `shiftL` 16 `div` a')
    0x08 -> do
      lift $ modify (+ 3)
      a <- memPartA acode
      (b, addr) <- memPartB bcode
      dcpuLens addr .= case a of
        0 -> 0
        _ -> b `mod` a
    0x09 -> do
      lift $ modify (+ 3)
      a <- fromIntegral <$> memPartA acode :: DCPUM Int16
      (b, addr) <- over _1 fromIntegral <$> memPartB bcode :: DCPUM (Int16, Either RegCode Word16)
      dcpuLens addr .= case a of
        0 -> 0
        _ -> fromIntegral (b `rem` a)
    0x0a -> do
      lift $ modify (+ 3)
      a <- memPartA acode
      (b, addr) <- memPartB bcode
      dcpuLens addr .= b .&. a
    0x0b -> do
      lift $ modify (+ 1)
      a <- memPartA acode
      (b, addr) <- memPartB bcode
      dcpuLens addr .= b .|. a
    0x0c -> do
      lift $ modify (+ 1)
      a <- memPartA acode
      (b, addr) <- memPartB bcode
      dcpuLens addr .= b `xor` a
    0x0d -> do
      lift $ modify (+ 1)
      a <- fromIntegral <$> memPartA acode
      (b, addr) <- memPartB bcode
      dcpuLens addr .= b `shiftR` a
      dcpuLens (Left EX) .= fromIntegral (fromIntegral b `shiftL` 16 `shiftR` a :: Word32)
    0x0e -> do
      lift $ modify (+ 1)
      a <- fromIntegral <$> memPartA acode
      (b, addr) <- over _1 fromIntegral <$> memPartB bcode :: DCPUM (Int16, Either RegCode Word16)
      dcpuLens addr .= fromIntegral (b `shiftR` a)
      dcpuLens (Left EX) .= fromIntegral (fromIntegral b `shiftL` 16 `shiftR` a :: Int32)
    0x0f -> do
      lift $ modify (+ 1)
      a <- fromIntegral <$> memPartA acode
      (b, addr) <- memPartB bcode
      dcpuLens addr .= b `shiftL` a
      dcpuLens (Left EX) .= fromIntegral (fromIntegral b `shiftL` a `shiftR` 16 :: Word32)
    0x10 -> do
      lift $ modify (+ 2)
      a <- memPartA acode
      (b, _) <- memPartB bcode
      unless (a .&. b /= 0) $ do
        lift $ modify (+ 1)
        skip <- sizeOfInstruction
        dcpuLens (Left PC) += skip
    0x11 -> do
      lift $ modify (+ 2)
      a <- memPartA acode 
      (b, _) <- memPartB bcode
      unless (a .&. b == 0) $ do
        lift $ modify (+ 1)
        skip <- sizeOfInstruction
        dcpuLens (Left PC) += skip
    0x12 -> do
      lift $ modify (+ 2)
      a <- memPartA acode
      (b, _) <- memPartB bcode
      unless (b == a) $ do
        lift $ modify (+ 1)
        skip <- sizeOfInstruction
        dcpuLens (Left PC) += skip
    0x13 -> do
      lift $ modify (+ 2)
      a <- memPartA acode
      (b, _) <- memPartB bcode
      unless (b /= a) $ do
        lift $ modify (+ 1)
        skip <- sizeOfInstruction
        dcpuLens (Left PC) += skip
    0x14 -> do
      lift $ modify (+ 2)
      a <- memPartA acode
      (b, _) <- memPartB bcode
      unless (b > a) $ do
        lift $ modify (+ 1)
        skip <- sizeOfInstruction
        dcpuLens (Left PC) += skip
    0x15 -> do
      lift $ modify (+ 2)
      a <- fromIntegral <$> memPartA acode :: DCPUM Int16
      (b, _) <- over _1 fromIntegral <$> memPartB bcode
      unless (b > a) $ do
        lift $ modify (+ 1)
        skip <- sizeOfInstruction
        dcpuLens (Left PC) += skip
    0x16 -> do
      lift $ modify (+ 2)
      a <- memPartA acode
      (b, _) <- memPartB bcode
      unless (b < a) $ do
        lift $ modify (+ 1)
        skip <- sizeOfInstruction
        dcpuLens (Left PC) += skip
    0x17 -> do
      lift $ modify (+ 2)
      a <- fromIntegral <$> memPartA acode :: DCPUM Int16
      (b, _) <- over _1 fromIntegral <$> memPartB bcode
      unless (b < a) $ do
        lift $ modify (+ 1)
        skip <- sizeOfInstruction
        dcpuLens (Left PC) += skip
    0x18 -> liftIO $ throwIO DCPUUndefinedOperation -- spec is eerily missing
    0x19 -> liftIO $ throwIO DCPUUndefinedOperation -- it's like someone stole these instructions from Notch's mind
    0x1a -> do
      lift $ modify (+ 3)
      a <- memPartA acode
      (b, addr) <- memPartB bcode
      c <- use . dcpuLens $ Left EX
      dcpuLens addr .= a + b + c
      dcpuLens (Left EX) .= if foldMap (Sum . fromIntegral :: Word16 -> Sum Integer) [a, b, c] > Sum 0xffff then 1 else 0
    0x1b -> do
      lift $ modify (+ 3)
      a <- memPartA acode
      (b, addr) <- memPartB bcode
      c <- use . dcpuLens $ Left EX
      dcpuLens addr .= b - a + c
      dcpuLens (Left EX) .= if b + c < a then -1 else 0
    0x1c -> liftIO $ throwIO DCPUUndefinedOperation -- the spec theif strikes again!
    0x1d -> liftIO $ throwIO DCPUUndefinedOperation -- someone call the police!
    0x1e -> do
      lift $ modify (+ 2)
      a <- memPartA acode
      (_, addr) <- memPartB bcode
      dcpuLens addr .= a
      dcpuLens (Left I) += 1
      dcpuLens (Left J) += 1
    0x1f -> do
      lift $ modify (+ 2)
      a <- memPartA acode
      (_, addr) <- memPartB bcode
      dcpuLens addr .= a
      dcpuLens (Left I) -= 1
      dcpuLens (Left J) -= 1
    _ -> liftIO $ throwIO DCPUUndefinedOperation
  hw <- use dcpuHardware
  mapM_ hardwareRefresh hw

runDCPU :: DCPUM a
runDCPU = step >> runDCPU