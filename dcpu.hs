{-# LANGUAGE TupleSections #-}
module Main where

import Control.Applicative
import Control.Arrow
--import Control.Concurrent
import Control.Lens
--import Control.Monad.IO.Class
import Control.Monad.State
import Control.Monad.Trans.Maybe
import Control.Monad.Writer

import Data.Array.Lens
import Data.Array.Unboxed
import Data.Bits
import Data.Foldable
import Data.Int
import Data.Maybe
import Data.Sequence hiding (zip)
--import Data.Sequence.Lens
import Data.Word

import Prelude hiding (elem)

data RegCode = A | B | C | X | Y | Z | I | J | SP | PC | EX | IA | N deriving (Eq, Ord, Enum)

instance Ix RegCode where
  range = uncurry enumFromTo
  index (a, b) x = fromJust . lookup x $ zip [a..b] [0..]
  inRange (a, b) x = x `elem` [a..b]

data DCPU = DCPU
  {ram :: UArray Word16 Word16
  ,reg :: UArray RegCode Word16
  ,isq :: Bool   
  ,irq :: Seq Word16
  ,hws :: Seq Hardware 
  }

-- things I need for Hardware
-- hardware info
-- a way of reading from the outside world and passing interrupts back to the DCPU- into the MVar provided?
-- a way of reading from the DCPU and changing the outside world (and how it reads)- Using an MVar?

data Hardware = Hardware
  {hardwareId :: Word32
  ,hardwareVersion :: Word16 
  ,hardwareManufacturer :: Word16 
  ,hardwareShoutbox :: Word16 -> DCPUM ()
  }

{-
lem1802 :: MVar [Word16] -> DCPUM Hardware
lem1802 shoutbox = undefined
-}
  
type DCPUM a = StateT DCPU (MaybeT (WriterT (Sum Integer) IO)) a

dcpuLens :: Either RegCode Word16 -> Simple Lens DCPU Word16
dcpuLens addr = either 
    (\rc -> lens reg (\dcpu x -> dcpu {reg = x}) . ix rc) 
    (\a -> lens ram (\dcpu x -> dcpu {ram = x}) . ix a) addr 
    
queueing :: Simple Lens DCPU Bool
queueing = lens isq (\dcpu q -> dcpu {isq = q})

queue :: Simple Lens DCPU (Seq Word16)
queue = lens irq (\dcpu q -> dcpu {irq = q})

memPartA :: Word16 -> DCPUM Word16
memPartA n | n <= 0x07 = use . dcpuLens . Left . toEnum $ fromIntegral n
           | n <= 0x0f = do
             addr <- use . dcpuLens . Left . toEnum $ fromIntegral (n - 8)
             use . dcpuLens $ Right addr
           | n <= 0x17 = do
             tell (Sum 1)
             pc <- use . dcpuLens $ Left PC
             dcpuLens (Left PC) += 1
             offset <- use . dcpuLens $ Right pc
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
             tell (Sum 1)
             pc <- use . dcpuLens $ Left PC
             dcpuLens (Left PC) += 1
             offset <- use . dcpuLens $ Right pc
             sp <- use . dcpuLens $ Left SP
             use . dcpuLens $ Right (sp + offset)
           | n == 0x1b = use . dcpuLens $ Left SP
           | n == 0x1c = use . dcpuLens $ Left PC
           | n == 0x1d = use . dcpuLens $ Left EX
           | n == 0x1e = do
             tell (Sum 1)
             pc <- use . dcpuLens $ Left PC
             dcpuLens (Left PC) += 1
             addr <- use . dcpuLens $ Right pc
             use . dcpuLens $ Right addr
           | n == 0x1f = do
             tell (Sum 1)
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
             tell (Sum 1)
             pc <- use . dcpuLens $ Left PC
             dcpuLens (Left PC) += 1
             offset <- use . dcpuLens $ Right pc
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
             tell (Sum 1)
             pc <- use . dcpuLens $ Left PC
             dcpuLens (Left PC) += 1
             offset <- use . dcpuLens $ Right pc
             sp <- use . dcpuLens $ Left SP
             uses (dcpuLens $ Right (sp + offset)) (, Right (sp + offset))
           | n == 0x1b = (, Left SP) <$> use (dcpuLens $ Left SP)
           | n == 0x1c = (, Left PC) <$> use (dcpuLens $ Left PC)
           | n == 0x1d = (, Left EX) <$> use (dcpuLens $ Left EX)
           | n == 0x1e = do
             tell (Sum 1)
             pc <- use . dcpuLens $ Left PC
             dcpuLens (Left PC) += 1
             addr <- use . dcpuLens $ Right pc
             uses (dcpuLens $ Right addr) (, Right addr)
           | n == 0x1f = do
             tell (Sum 1)
             pc <- use . dcpuLens $ Left PC
             dcpuLens (Left PC) += 1
             uses (dcpuLens $ Right pc) (, Right pc)
           | otherwise = return (n - 33, Left N)

step :: DCPUM ()
step = do
  pc <- use . dcpuLens $ Left PC
  dcpuLens (Left PC) += 1
  op <- use . dcpuLens $ Right pc
  let opcode = op .&. 0x1F
      bcode = (op `shiftR` 5) .&. 0x1F
      acode = op `shiftR` 10
  case opcode of
    0x00 -> case bcode of
      0x00 -> undefined -- reserved for future expansion
      0x01 -> do
        tell (Sum 3)
        a <- memPartA acode
        pc1 <- use . dcpuLens $ Left PC
        dcpuLens (Left SP) -= 1
        sp <- use . dcpuLens $ Left SP
        dcpuLens (Right sp) .= pc1
        dcpuLens (Left PC) .= a
      0x02 -> undefined
      0x03 -> undefined
      0x04 -> undefined
      0x05 -> undefined
      0x06 -> undefined
      0x07 -> undefined
      0x08 -> do
        tell (Sum 4)
        a <- memPartA acode
        interrupt a
      0x09 -> do
        tell (Sum 1)
        (_, addr) <- memPartB acode
        ia <- use . dcpuLens $ Left IA
        dcpuLens addr .= ia
      0x0a -> do
        tell (Sum 1)
        a <- memPartA acode
        dcpuLens (Left IA) .= a
      0x0b -> do
        tell (Sum 3)
        queueing .= False
        sp1 <- use . dcpuLens $ Left SP
        dcpuLens (Left SP) += 1
        a <- use . dcpuLens $ Right sp1
        dcpuLens (Left A) .= a
        sp2 <- use . dcpuLens $ Left SP
        dcpuLens (Left SP) += 1
        pc1 <- use . dcpuLens $ Right sp2
        dcpuLens (Left PC) .= pc1
      0x0c -> do
        tell (Sum 2)
        a <- memPartA acode
        queueing .= (a /= 0)
      0x0d -> undefined
      0x0e -> undefined
      0x0f -> undefined
      0x10 -> do
        tell (Sum 2)
      _ -> undefined  
    0x01 -> do
      tell (Sum 1)
      a <- memPartA acode
      (_, addr) <- memPartB bcode
      dcpuLens addr .= a
    0x02 -> do
      tell (Sum 2)
      a <- memPartA acode
      (b, addr) <- memPartB bcode
      dcpuLens addr .= a + b
      dcpuLens (Left EX) .= if (a + b) < a then 1 else 0
    0x03 -> do
      tell (Sum 2)
      a <- memPartA acode
      (b, addr) <- memPartB bcode
      dcpuLens addr .= b - a
      dcpuLens (Left EX) .= if (a > b) then -1 else 0
    0x04 -> do
      tell (Sum 2)
      a <- memPartA acode
      (b, addr) <- memPartB bcode
      dcpuLens addr .= b * a
      let a', b' :: Word32
          a' = fromIntegral a
          b' = fromIntegral b
      dcpuLens (Left EX) .= fromIntegral ((a' * b') `shiftR` 16)
    0x05 -> do
      tell (Sum 2)
      a <- fromIntegral <$> memPartA acode :: DCPUM Int16
      (b, addr) <- first fromIntegral <$> memPartB bcode :: DCPUM (Int16, Either RegCode Word16)
      dcpuLens addr .= fromIntegral (b * a)
      let a', b' :: Int32
          a' = fromIntegral a
          b' = fromIntegral b
      dcpuLens (Left EX) .= fromIntegral ((a' * b') `shiftR` 16)
    0x06 -> do
      tell (Sum 3)
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
      tell (Sum 3)
      a <- fromIntegral <$> memPartA acode :: DCPUM Int16
      (b, addr) <- first fromIntegral <$> memPartB bcode :: DCPUM (Int16, Either RegCode Word16)
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
      tell (Sum 3)
      a <- memPartA acode
      (b, addr) <- memPartB bcode
      dcpuLens addr .= case a of
        0 -> 0
        _ -> b `mod` a
    0x09 -> do
      tell (Sum 3)
      a <- fromIntegral <$> memPartA acode :: DCPUM Int16
      (b, addr) <- first fromIntegral <$> memPartB bcode :: DCPUM (Int16, Either RegCode Word16)
      dcpuLens addr .= case a of
        0 -> 0
        _ -> fromIntegral (b `rem` a)
    0x0a -> do
      tell (Sum 1)
      a <- memPartA acode
      (b, addr) <- memPartB bcode
      dcpuLens addr .= b .&. a
    0x0b -> do
      tell (Sum 1)
      a <- memPartA acode
      (b, addr) <- memPartB bcode
      dcpuLens addr .= b .|. a
    0x0c -> do
      tell (Sum 1)
      a <- memPartA acode
      (b, addr) <- memPartB bcode
      dcpuLens addr .= b `xor` a
    0x0d -> do
      tell (Sum 1)
      a <- fromIntegral <$> memPartA acode
      (b, addr) <- memPartB bcode
      dcpuLens addr .= b `shiftR` a
      dcpuLens (Left EX) .= fromIntegral (fromIntegral b `shiftL` 16 `shiftR` a :: Word32)
    0x0e -> do
      tell (Sum 1)
      a <- fromIntegral <$> memPartA acode
      (b, addr) <- first fromIntegral <$> memPartB bcode :: DCPUM (Int16, Either RegCode Word16)
      dcpuLens addr .= fromIntegral (b `shiftR` a)
      dcpuLens (Left EX) .= fromIntegral (fromIntegral b `shiftL` 16 `shiftR` a :: Int32)
    0x0f -> do
      tell (Sum 1)
      a <- fromIntegral <$> memPartA acode
      (b, addr) <- memPartB bcode
      dcpuLens addr .= b `shiftL` a
      dcpuLens (Left EX) .= fromIntegral (fromIntegral b `shiftL` a `shiftR` 16 :: Word32)
    0x10 -> do
      tell (Sum 2)
      a <- memPartA acode
      (b, _) <- memPartB bcode
      unless (a .&. b /= 0) $ do
        tell (Sum 1)
        skip <- sizeOfInstruction
        dcpuLens (Left PC) += skip
    0x11 -> do
      tell (Sum 2)
      a <- memPartA acode 
      (b, _) <- memPartB bcode
      unless (a .&. b == 0) $ do
        tell (Sum 1)
        skip <- sizeOfInstruction
        dcpuLens (Left PC) += skip
    0x12 -> do
      tell (Sum 2)
      a <- memPartA acode
      (b, _) <- memPartB bcode
      unless (b == a) $ do
        tell (Sum 1)
        skip <- sizeOfInstruction
        dcpuLens (Left PC) += skip
    0x13 -> do
      tell (Sum 2)
      a <- memPartA acode
      (b, _) <- memPartB bcode
      unless (b /= a) $ do
        tell (Sum 1)
        skip <- sizeOfInstruction
        dcpuLens (Left PC) += skip
    0x14 -> do
      tell (Sum 2)
      a <- memPartA acode
      (b, _) <- memPartB bcode
      unless (b > a) $ do
        tell (Sum 1)
        skip <- sizeOfInstruction
        dcpuLens (Left PC) += skip
    0x15 -> do
      tell (Sum 2)
      a <- fromIntegral <$> memPartA acode :: DCPUM Int16
      (b, _) <- first fromIntegral <$> memPartB bcode
      unless (b > a) $ do
        tell (Sum 1)
        skip <- sizeOfInstruction
        dcpuLens (Left PC) += skip
    0x16 -> do
      tell (Sum 2)
      a <- memPartA acode
      (b, _) <- memPartB bcode
      unless (b < a) $ do
        tell (Sum 1)
        skip <- sizeOfInstruction
        dcpuLens (Left PC) += skip
    0x17 -> do
      tell (Sum 2)
      a <- fromIntegral <$> memPartA acode :: DCPUM Int16
      (b, _) <- first fromIntegral <$> memPartB bcode
      unless (b < a) $ do
        tell (Sum 1)
        skip <- sizeOfInstruction
        dcpuLens (Left PC) += skip
    0x18 -> undefined -- spec is eerily missing
    0x19 -> undefined -- it's like someone stole these instructions from Notch's mind
    0x1a -> do
      tell (Sum 3)
      a <- memPartA acode
      (b, addr) <- memPartB bcode
      c <- use . dcpuLens $ Left EX
      dcpuLens addr .= a + b + c
      dcpuLens (Left EX) .= if foldMap (Sum . fromIntegral :: Word16 -> Sum Integer) [a, b, c] > Sum 0xffff then 1 else 0
    0x1b -> do
      tell (Sum 3)
      a <- memPartA acode
      (b, addr) <- memPartB bcode
      c <- use . dcpuLens $ Left EX
      dcpuLens addr .= b - a + c
      dcpuLens (Left EX) .= if b + c < a then -1 else 0
    0x1c -> undefined -- the spec theif strikes again!
    0x1d -> undefined -- someone call the police!
    0x1e -> do
      tell (Sum 2)
      a <- memPartA acode
      (_, addr) <- memPartB bcode
      dcpuLens addr .= a
      dcpuLens (Left I) += 1
      dcpuLens (Left J) += 1
    0x1f -> do
      tell (Sum 2)
      a <- memPartA acode
      (_, addr) <- memPartB bcode
      dcpuLens addr .= a
      dcpuLens (Left I) -= 1
      dcpuLens (Left J) -= 1
    _ -> undefined

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
  q <- use queueing
  if q
    then queue %= (msg <|)
    else do
      ia <- use . dcpuLens $ Left IA
      unless (ia == 0) $ do
        queueing .= True
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