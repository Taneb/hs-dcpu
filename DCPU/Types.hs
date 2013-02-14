{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TemplateHaskell #-}

{-# OPTIONS_GHC -Wall #-}
module DCPU.Types where

import Control.Arrow
import Control.Exception
import Control.Lens
import Control.Monad.State.Strict
import Control.Monad.Trans.Maybe

import Data.Function.Pointless
import Data.Sequence (Seq)
import Data.Typeable
import Data.Word

data RegCode = A | B | C | X | Y | Z | I | J | SP | PC | EX | IA | N deriving (Eq, Ord, Enum)

data DCPU = DCPU
  {_dcpuRam :: Word16 -> Word16
  ,_dcpuRegisters :: RegCode -> Word16
  ,_dcpuIsInterruptQueueing :: Bool   
  ,_dcpuInterruptQueue :: Seq Word16
  ,_dcpuHardware :: Seq Hardware -- maybe should be Map Word16 Hardware?
  }

-- things I need for Hardware
-- hardware info
-- a way of reading from the outside world and passing interrupts back to the DCPU- into the MVar provided?
-- a way of reading from the DCPU and changing the outside world (and how it reads)- Using an MVar?

data Hardware = Hardware
  {hardwareID :: Word32
  ,hardwareVersion :: Word16 
  ,hardwareManufacturer :: Word32
  ,hardwareInterrupt :: DCPUM ()
  ,hardwareRefresh :: DCPUM ()
  }

type DCPUM a = StateT DCPU (MaybeT (StateT Integer IO)) a

makeLenses ''DCPU

dcpuLens :: Either RegCode Word16 -> Lens' DCPU Word16
dcpuLens = dcpuRegisters .: ix ||| dcpuRam .: ix

data DCPUException = DCPUUndefinedOperation | DCPUInterruptOverflow | NoSuchHardware | DCPUSpontaneouslyCombusted deriving (Show, Typeable)

instance Exception DCPUException