{-# OPTIONS_GHC -fno-warn-overlapping-patterns #-}
{-# OPTIONS -fglasgow-exts -cpp #-}
module SinBrainiac where

import Control.Monad.Identity
import Control.Monad.State
import Control.Monad.Writer
import qualified Data.Foldable as DF
import qualified Data.Sequence as DS

import LexBrainiac
#if __GLASGOW_HASKELL__ >= 503
import qualified Data.Array as Happy_Data_Array
#else
import qualified Array as Happy_Data_Array
#endif
#if __GLASGOW_HASKELL__ >= 503
import qualified GHC.Exts as Happy_GHC_Exts
#else
import qualified GlaExts as Happy_GHC_Exts
#endif

-- parser produced by Happy Version 1.18.4

newtype HappyAbsSyn  = HappyAbsSyn HappyAny
#if __GLASGOW_HASKELL__ >= 607
type HappyAny = Happy_GHC_Exts.Any
#else
type HappyAny = forall a . a
#endif
happyIn4 :: (Inst) -> (HappyAbsSyn )
happyIn4 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn4 #-}
happyOut4 :: (HappyAbsSyn ) -> (Inst)
happyOut4 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut4 #-}
happyIn5 :: ([Declaracion]) -> (HappyAbsSyn )
happyIn5 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn5 #-}
happyOut5 :: (HappyAbsSyn ) -> ([Declaracion])
happyOut5 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut5 #-}
happyIn6 :: ([Declaracion]) -> (HappyAbsSyn )
happyIn6 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn6 #-}
happyOut6 :: (HappyAbsSyn ) -> ([Declaracion])
happyOut6 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut6 #-}
happyIn7 :: ([VarName]) -> (HappyAbsSyn )
happyIn7 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn7 #-}
happyOut7 :: (HappyAbsSyn ) -> ([VarName])
happyOut7 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut7 #-}
happyIn8 :: (Tipo) -> (HappyAbsSyn )
happyIn8 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn8 #-}
happyOut8 :: (HappyAbsSyn ) -> (Tipo)
happyOut8 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut8 #-}
happyIn9 :: (OpComp) -> (HappyAbsSyn )
happyIn9 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn9 #-}
happyOut9 :: (HappyAbsSyn ) -> (OpComp)
happyOut9 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut9 #-}
happyIn10 :: (OpBin) -> (HappyAbsSyn )
happyIn10 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn10 #-}
happyOut10 :: (HappyAbsSyn ) -> (OpBin)
happyOut10 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut10 #-}
happyIn11 :: (OpBin) -> (HappyAbsSyn )
happyIn11 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn11 #-}
happyOut11 :: (HappyAbsSyn ) -> (OpBin)
happyOut11 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut11 #-}
happyIn12 :: (OpUn) -> (HappyAbsSyn )
happyIn12 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn12 #-}
happyOut12 :: (HappyAbsSyn ) -> (OpUn)
happyOut12 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut12 #-}
happyIn13 :: ([Inst]) -> (HappyAbsSyn )
happyIn13 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn13 #-}
happyOut13 :: (HappyAbsSyn ) -> ([Inst])
happyOut13 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut13 #-}
happyIn14 :: (Inst) -> (HappyAbsSyn )
happyIn14 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn14 #-}
happyOut14 :: (HappyAbsSyn ) -> (Inst)
happyOut14 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut14 #-}
happyIn15 :: (Exp) -> (HappyAbsSyn )
happyIn15 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn15 #-}
happyOut15 :: (HappyAbsSyn ) -> (Exp)
happyOut15 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut15 #-}
happyIn16 :: (Exp) -> (HappyAbsSyn )
happyIn16 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn16 #-}
happyOut16 :: (HappyAbsSyn ) -> (Exp)
happyOut16 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut16 #-}
happyIn17 :: (Exp) -> (HappyAbsSyn )
happyIn17 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn17 #-}
happyOut17 :: (HappyAbsSyn ) -> (Exp)
happyOut17 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut17 #-}
happyIn18 :: (Exp) -> (HappyAbsSyn )
happyIn18 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn18 #-}
happyOut18 :: (HappyAbsSyn ) -> (Exp)
happyOut18 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut18 #-}
happyIn19 :: ([B_Inst]) -> (HappyAbsSyn )
happyIn19 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn19 #-}
happyOut19 :: (HappyAbsSyn ) -> ([B_Inst])
happyOut19 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut19 #-}
happyIn20 :: (B_Inst) -> (HappyAbsSyn )
happyIn20 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn20 #-}
happyOut20 :: (HappyAbsSyn ) -> (B_Inst)
happyOut20 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut20 #-}
happyInTok :: (Token) -> (HappyAbsSyn )
happyInTok x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyInTok #-}
happyOutTok :: (HappyAbsSyn ) -> (Token)
happyOutTok x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOutTok #-}


happyActOffsets :: HappyAddr
happyActOffsets = HappyA# "\x50\x00\xf2\x00\xeb\x00\xba\x00\x01\x00\x30\x00\xdc\x00\xcb\x00\x45\x00\x07\x01\x00\x00\xab\x00\x00\x00\xd4\x00\x01\x00\x30\x00\x20\x00\x0c\x00\xcd\x00\x0c\x00\x00\x00\x00\x00\x0c\x00\x00\x00\x00\x00\x0c\x00\x4d\x00\x00\x00\xd6\x00\xb0\x00\xa6\x00\xa4\x00\xb1\x00\x7e\x00\xb1\x00\x01\x00\x7f\x00\x4d\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x59\x00\x00\x00\x71\x00\xfe\x00\x00\x00\x98\x00\x61\x00\x3c\x00\x94\x00\x8f\x00\x86\x00\x5a\x00\x0c\x00\x0c\x00\x00\x00\x00\x00\x00\x00\x0c\x00\x00\x00\x00\x00\x00\x00\x00\x00\x0c\x00\x01\x00\x00\x00\x00\x00\x00\x00\xfe\x00\x04\x01\x00\x00\xfe\x00\x01\x00\x00\x00\x01\x00\x0c\x00\x0c\x00\x0c\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x37\x00\x76\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x0c\x00\x11\x00\xfe\x00\x3a\x00\xf7\xff\x6c\x00\x6a\x00\x00\x00\x00\x00\x0c\x00\x01\x00\x00\x00\x01\x00\xfe\x00\x69\x00\x42\x00\x1e\x00\x01\x00\x00\x00\x00\x00\x38\x00\x00\x00\x00\x00"#

happyGotoOffsets :: HappyAddr
happyGotoOffsets = HappyA# "\x40\x00\x00\x00\x7d\x00\x00\x00\xca\x00\x1f\x00\x00\x00\x00\x00\x29\x00\x24\x00\x00\x00\x00\x00\x00\x00\x75\x00\xc3\x00\xd1\x00\x0a\x01\x5f\x00\x00\x00\x03\x01\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\xf9\x00\x41\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x26\x00\x22\x00\x19\x00\xbc\x00\x00\x00\x3e\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x04\x00\x04\x00\x00\x00\x00\x00\x31\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\xf6\x00\x03\x00\x00\x00\x00\x00\x00\x00\x3d\x00\x00\x00\x00\x00\x00\x00\x00\x00\xef\x00\xb5\x00\x00\x00\x00\x00\x00\x00\x04\x00\xff\xff\x00\x00\x02\x00\xae\x00\x00\x00\xa7\x00\xec\x00\xe5\x00\xe2\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xa0\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xdb\x00\x00\x00\x02\x00\x02\x00\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\xd8\x00\x99\x00\x00\x00\x92\x00\x02\x00\x00\x00\x00\x00\x02\x00\x8b\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00"#

happyDefActions :: HappyAddr
happyDefActions = HappyA# "\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xe4\xff\x00\x00\xd3\xff\xd1\xff\xd0\xff\xcf\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xcd\xff\xcc\xff\x00\x00\xe7\xff\xe6\xff\x00\x00\x00\x00\xe5\xff\x00\x00\xfc\xff\x00\x00\xf9\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xc9\xff\xc2\xff\xc3\xff\xc7\xff\xc6\xff\xc5\xff\xc4\xff\x00\x00\xd0\xff\x00\x00\xda\xff\xd9\xff\x00\x00\xd5\xff\x00\x00\xd0\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xea\xff\xe9\xff\xe8\xff\x00\x00\xee\xff\xed\xff\xec\xff\xeb\xff\x00\x00\x00\x00\xfd\xff\xce\xff\xe3\xff\xd7\xff\xd4\xff\xd2\xff\xe2\xff\x00\x00\xdb\xff\x00\x00\x00\x00\x00\x00\x00\x00\xf1\xff\xf2\xff\xef\xff\xf0\xff\xf4\xff\xf3\xff\x00\x00\xca\xff\xcb\xff\xc8\xff\x00\x00\x00\x00\xfb\xff\xfa\xff\xf7\xff\xf6\xff\xf5\xff\xf8\xff\xfe\xff\x00\x00\x00\x00\xd6\xff\x00\x00\x00\x00\x00\x00\x00\x00\xdc\xff\xdf\xff\x00\x00\x00\x00\xe1\xff\x00\x00\xd8\xff\x00\x00\x00\x00\x00\x00\x00\x00\xdd\xff\xe0\xff\x00\x00\xde\xff"#

happyCheck :: HappyAddr
happyCheck = HappyA# "\xff\xff\x0a\x00\x01\x00\x02\x00\x03\x00\x04\x00\x07\x00\x06\x00\x06\x00\x08\x00\x06\x00\x08\x00\x0b\x00\x01\x00\x02\x00\x0e\x00\x0f\x00\x0e\x00\x1b\x00\x1c\x00\x13\x00\x14\x00\x05\x00\x20\x00\x21\x00\x18\x00\x01\x00\x02\x00\x03\x00\x1c\x00\x0d\x00\x13\x00\x14\x00\x01\x00\x02\x00\x22\x00\x18\x00\x07\x00\x04\x00\x08\x00\x1c\x00\x03\x00\x29\x00\x07\x00\x2b\x00\x0e\x00\x22\x00\x06\x00\x2f\x00\x01\x00\x02\x00\x13\x00\x14\x00\x29\x00\x05\x00\x06\x00\x18\x00\x1b\x00\x1c\x00\x2f\x00\x1c\x00\x05\x00\x20\x00\x21\x00\x00\x00\x07\x00\x22\x00\x13\x00\x14\x00\x08\x00\x0a\x00\x05\x00\x18\x00\x29\x00\x0d\x00\x0e\x00\x1c\x00\x0f\x00\x10\x00\x2f\x00\x0f\x00\x10\x00\x22\x00\x03\x00\x04\x00\x1b\x00\x1c\x00\x1b\x00\x1c\x00\x29\x00\x20\x00\x21\x00\x20\x00\x21\x00\x04\x00\x2f\x00\x1b\x00\x1c\x00\x15\x00\x16\x00\x2d\x00\x20\x00\x21\x00\x08\x00\x1b\x00\x1c\x00\x0b\x00\x0c\x00\x0d\x00\x0e\x00\x05\x00\x05\x00\x23\x00\x05\x00\x25\x00\x2e\x00\x1b\x00\x1c\x00\x01\x00\x02\x00\x03\x00\x20\x00\x21\x00\x05\x00\x1b\x00\x1c\x00\x01\x00\x02\x00\x03\x00\x20\x00\x21\x00\x2a\x00\x23\x00\x24\x00\x25\x00\x26\x00\x27\x00\x28\x00\x19\x00\x05\x00\x1b\x00\x1c\x00\x10\x00\x11\x00\x12\x00\x20\x00\x21\x00\x08\x00\x09\x00\x0a\x00\x07\x00\x0c\x00\x0d\x00\x0e\x00\x08\x00\x09\x00\x0a\x00\x09\x00\x0c\x00\x0d\x00\x0e\x00\x08\x00\x09\x00\x0a\x00\x0c\x00\x0c\x00\x0d\x00\x0e\x00\x08\x00\x09\x00\x0a\x00\x2c\x00\x0c\x00\x0d\x00\x0e\x00\x08\x00\x09\x00\x0a\x00\x01\x00\x0c\x00\x0d\x00\x0e\x00\x08\x00\x09\x00\x0a\x00\x15\x00\x0c\x00\x0d\x00\x0e\x00\x08\x00\x09\x00\x0a\x00\x1a\x00\x0c\x00\x0d\x00\x0e\x00\x08\x00\x09\x00\x0a\x00\x17\x00\x0c\x00\x0d\x00\x0e\x00\x08\x00\x09\x00\x0a\x00\x01\x00\x0c\x00\x0d\x00\x0e\x00\x08\x00\x09\x00\x0a\x00\x01\x00\x0c\x00\x0d\x00\x0e\x00\x08\x00\x04\x00\x30\x00\x0b\x00\x0c\x00\x0d\x00\x0e\x00\x08\x00\x05\x00\x17\x00\x08\x00\x0c\x00\x0d\x00\x0e\x00\x0c\x00\x0d\x00\x0e\x00\x08\x00\x31\x00\x01\x00\x08\x00\x0c\x00\x0d\x00\x0e\x00\x0c\x00\x0d\x00\x0e\x00\x08\x00\x03\x00\xff\xff\x08\x00\x0c\x00\x0d\x00\x0e\x00\x0c\x00\x0d\x00\x0e\x00\x08\x00\xff\xff\xff\xff\x08\x00\x0c\x00\x0d\x00\x0e\x00\x0c\x00\x0d\x00\x0e\x00\x08\x00\xff\xff\xff\xff\x08\x00\x0c\x00\x0d\x00\x0e\x00\x0c\x00\x0d\x00\x0e\x00\x08\x00\xff\xff\xff\xff\xff\xff\x0c\x00\x0d\x00\x0e\x00\x1b\x00\x1c\x00\xff\xff\xff\xff\xff\xff\x20\x00\x21\x00\xff\xff\x1d\x00\x1e\x00\x1f\x00\x1d\x00\x1e\x00\x1f\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff"#

happyTable :: HappyAddr
happyTable = HappyA# "\x00\x00\x6e\x00\x0c\x00\x0d\x00\x0e\x00\x0f\x00\x39\x00\x10\x00\x3d\x00\x11\x00\x3d\x00\x05\x00\x12\x00\x2e\x00\x0d\x00\x13\x00\x14\x00\x49\x00\x3f\x00\x40\x00\x15\x00\x16\x00\x70\x00\x41\x00\x42\x00\x17\x00\x5d\x00\x1d\x00\x1e\x00\x18\x00\x71\x00\x15\x00\x16\x00\x35\x00\x0d\x00\x19\x00\x17\x00\x76\x00\x5e\x00\x05\x00\x18\x00\x62\x00\x1a\x00\x39\x00\x1b\x00\x45\x00\x19\x00\x3d\x00\x1c\x00\x2e\x00\x0d\x00\x15\x00\x16\x00\x1a\x00\x50\x00\x3d\x00\x17\x00\x3f\x00\x40\x00\x1c\x00\x18\x00\x7a\x00\x41\x00\x42\x00\x03\x00\x6f\x00\x19\x00\x15\x00\x16\x00\x05\x00\x50\x00\x77\x00\x17\x00\x1a\x00\x48\x00\x0a\x00\x18\x00\x5a\x00\x25\x00\x1c\x00\x24\x00\x25\x00\x19\x00\x03\x00\x05\x00\x3f\x00\x40\x00\x3f\x00\x40\x00\x1a\x00\x41\x00\x42\x00\x41\x00\x42\x00\x4c\x00\x1c\x00\x3f\x00\x40\x00\x27\x00\x28\x00\x65\x00\x41\x00\x42\x00\x05\x00\x29\x00\x2a\x00\x31\x00\x32\x00\x09\x00\x0a\x00\x78\x00\x6c\x00\x2b\x00\x6d\x00\x2c\x00\x43\x00\x3f\x00\x40\x00\x37\x00\x1d\x00\x1e\x00\x41\x00\x42\x00\x64\x00\x3f\x00\x40\x00\x1c\x00\x1d\x00\x1e\x00\x41\x00\x42\x00\x5a\x00\x52\x00\x53\x00\x54\x00\x55\x00\x56\x00\x57\x00\x59\x00\x4d\x00\x3f\x00\x40\x00\x60\x00\x61\x00\x62\x00\x41\x00\x42\x00\x05\x00\x78\x00\x07\x00\x4e\x00\x08\x00\x09\x00\x0a\x00\x05\x00\x72\x00\x07\x00\x4f\x00\x08\x00\x09\x00\x0a\x00\x05\x00\x73\x00\x07\x00\x58\x00\x08\x00\x09\x00\x0a\x00\x05\x00\x65\x00\x07\x00\x5c\x00\x08\x00\x09\x00\x0a\x00\x05\x00\x69\x00\x07\x00\x20\x00\x08\x00\x09\x00\x0a\x00\x05\x00\x6a\x00\x07\x00\x21\x00\x08\x00\x09\x00\x0a\x00\x05\x00\x46\x00\x07\x00\x22\x00\x08\x00\x09\x00\x0a\x00\x05\x00\x5c\x00\x07\x00\x23\x00\x08\x00\x09\x00\x0a\x00\x05\x00\x36\x00\x07\x00\x31\x00\x08\x00\x09\x00\x0a\x00\x05\x00\x06\x00\x07\x00\x20\x00\x08\x00\x09\x00\x0a\x00\x05\x00\x24\x00\x39\x00\x35\x00\x32\x00\x09\x00\x0a\x00\x05\x00\x45\x00\x44\x00\x05\x00\x74\x00\x09\x00\x0a\x00\x71\x00\x09\x00\x0a\x00\x05\x00\xff\xff\x20\x00\x05\x00\x66\x00\x09\x00\x0a\x00\x67\x00\x09\x00\x0a\x00\x05\x00\x03\x00\x00\x00\x05\x00\x68\x00\x09\x00\x0a\x00\x47\x00\x09\x00\x0a\x00\x05\x00\x00\x00\x00\x00\x05\x00\x4a\x00\x09\x00\x0a\x00\x2c\x00\x09\x00\x0a\x00\x05\x00\x00\x00\x00\x00\x05\x00\x2e\x00\x09\x00\x0a\x00\x2f\x00\x09\x00\x0a\x00\x05\x00\x00\x00\x00\x00\x00\x00\x33\x00\x09\x00\x0a\x00\x3f\x00\x40\x00\x00\x00\x00\x00\x00\x00\x41\x00\x42\x00\x00\x00\x3b\x00\x3c\x00\x3d\x00\x3b\x00\x3c\x00\x3d\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00"#

happyReduceArr = Happy_Data_Array.array (1, 61) [
	(1 , happyReduce_1),
	(2 , happyReduce_2),
	(3 , happyReduce_3),
	(4 , happyReduce_4),
	(5 , happyReduce_5),
	(6 , happyReduce_6),
	(7 , happyReduce_7),
	(8 , happyReduce_8),
	(9 , happyReduce_9),
	(10 , happyReduce_10),
	(11 , happyReduce_11),
	(12 , happyReduce_12),
	(13 , happyReduce_13),
	(14 , happyReduce_14),
	(15 , happyReduce_15),
	(16 , happyReduce_16),
	(17 , happyReduce_17),
	(18 , happyReduce_18),
	(19 , happyReduce_19),
	(20 , happyReduce_20),
	(21 , happyReduce_21),
	(22 , happyReduce_22),
	(23 , happyReduce_23),
	(24 , happyReduce_24),
	(25 , happyReduce_25),
	(26 , happyReduce_26),
	(27 , happyReduce_27),
	(28 , happyReduce_28),
	(29 , happyReduce_29),
	(30 , happyReduce_30),
	(31 , happyReduce_31),
	(32 , happyReduce_32),
	(33 , happyReduce_33),
	(34 , happyReduce_34),
	(35 , happyReduce_35),
	(36 , happyReduce_36),
	(37 , happyReduce_37),
	(38 , happyReduce_38),
	(39 , happyReduce_39),
	(40 , happyReduce_40),
	(41 , happyReduce_41),
	(42 , happyReduce_42),
	(43 , happyReduce_43),
	(44 , happyReduce_44),
	(45 , happyReduce_45),
	(46 , happyReduce_46),
	(47 , happyReduce_47),
	(48 , happyReduce_48),
	(49 , happyReduce_49),
	(50 , happyReduce_50),
	(51 , happyReduce_51),
	(52 , happyReduce_52),
	(53 , happyReduce_53),
	(54 , happyReduce_54),
	(55 , happyReduce_55),
	(56 , happyReduce_56),
	(57 , happyReduce_57),
	(58 , happyReduce_58),
	(59 , happyReduce_59),
	(60 , happyReduce_60),
	(61 , happyReduce_61)
	]

happy_n_terms = 50 :: Int
happy_n_nonterms = 17 :: Int

happyReduce_1 = happyReduce 5# 0# happyReduction_1
happyReduction_1 (happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut5 happy_x_2 of { happy_var_2 -> 
	case happyOut13 happy_x_4 of { happy_var_4 -> 
	happyIn4
		 (I_Declare happy_var_2 happy_var_4
	) `HappyStk` happyRest}}

happyReduce_2 = happySpecReduce_3  0# happyReduction_2
happyReduction_2 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut13 happy_x_2 of { happy_var_2 -> 
	happyIn4
		 (I_Declare [] happy_var_2
	)}

happyReduce_3 = happySpecReduce_1  1# happyReduction_3
happyReduction_3 happy_x_1
	 =  case happyOut6 happy_x_1 of { happy_var_1 -> 
	happyIn5
		 (happy_var_1
	)}

happyReduce_4 = happySpecReduce_3  1# happyReduction_4
happyReduction_4 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut6 happy_x_1 of { happy_var_1 -> 
	case happyOut5 happy_x_3 of { happy_var_3 -> 
	happyIn5
		 (happy_var_1 ++ happy_var_3
	)}}

happyReduce_5 = happySpecReduce_3  2# happyReduction_5
happyReduction_5 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut7 happy_x_1 of { happy_var_1 -> 
	case happyOut8 happy_x_3 of { happy_var_3 -> 
	happyIn6
		 (map (\v -> Decl v happy_var_3) happy_var_1
	)}}

happyReduce_6 = happySpecReduce_1  3# happyReduction_6
happyReduction_6 happy_x_1
	 =  case happyOutTok happy_x_1 of { (TkIdent _ happy_var_1) -> 
	happyIn7
		 ([happy_var_1]
	)}

happyReduce_7 = happySpecReduce_3  3# happyReduction_7
happyReduction_7 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOutTok happy_x_1 of { (TkIdent _ happy_var_1) -> 
	case happyOut7 happy_x_3 of { happy_var_3 -> 
	happyIn7
		 (happy_var_1 : happy_var_3
	)}}

happyReduce_8 = happySpecReduce_1  4# happyReduction_8
happyReduction_8 happy_x_1
	 =  happyIn8
		 (Tipo_Boolean
	)

happyReduce_9 = happySpecReduce_1  4# happyReduction_9
happyReduction_9 happy_x_1
	 =  happyIn8
		 (Tipo_Integer
	)

happyReduce_10 = happySpecReduce_1  4# happyReduction_10
happyReduction_10 happy_x_1
	 =  happyIn8
		 (Tipo_Tape
	)

happyReduce_11 = happySpecReduce_1  5# happyReduction_11
happyReduction_11 happy_x_1
	 =  happyIn9
		 (Op_Eq
	)

happyReduce_12 = happySpecReduce_1  5# happyReduction_12
happyReduction_12 happy_x_1
	 =  happyIn9
		 (Op_Neq
	)

happyReduce_13 = happySpecReduce_1  5# happyReduction_13
happyReduction_13 happy_x_1
	 =  happyIn9
		 (Op_Leq
	)

happyReduce_14 = happySpecReduce_1  5# happyReduction_14
happyReduction_14 happy_x_1
	 =  happyIn9
		 (Op_Lt
	)

happyReduce_15 = happySpecReduce_1  5# happyReduction_15
happyReduction_15 happy_x_1
	 =  happyIn9
		 (Op_Geq
	)

happyReduce_16 = happySpecReduce_1  5# happyReduction_16
happyReduction_16 happy_x_1
	 =  happyIn9
		 (Op_Gt
	)

happyReduce_17 = happySpecReduce_1  6# happyReduction_17
happyReduction_17 happy_x_1
	 =  happyIn10
		 (Op_Sum
	)

happyReduce_18 = happySpecReduce_1  6# happyReduction_18
happyReduction_18 happy_x_1
	 =  happyIn10
		 (Op_Res
	)

happyReduce_19 = happySpecReduce_1  6# happyReduction_19
happyReduction_19 happy_x_1
	 =  happyIn10
		 (Op_Con
	)

happyReduce_20 = happySpecReduce_1  6# happyReduction_20
happyReduction_20 happy_x_1
	 =  happyIn10
		 (Op_Dis
	)

happyReduce_21 = happySpecReduce_1  7# happyReduction_21
happyReduction_21 happy_x_1
	 =  happyIn11
		 (Op_Mul
	)

happyReduce_22 = happySpecReduce_1  7# happyReduction_22
happyReduction_22 happy_x_1
	 =  happyIn11
		 (Op_Div
	)

happyReduce_23 = happySpecReduce_1  7# happyReduction_23
happyReduction_23 happy_x_1
	 =  happyIn11
		 (Op_Mod
	)

happyReduce_24 = happySpecReduce_1  8# happyReduction_24
happyReduction_24 happy_x_1
	 =  happyIn12
		 (Op_NegArit
	)

happyReduce_25 = happySpecReduce_1  8# happyReduction_25
happyReduction_25 happy_x_1
	 =  happyIn12
		 (Op_NegBool
	)

happyReduce_26 = happySpecReduce_1  8# happyReduction_26
happyReduction_26 happy_x_1
	 =  happyIn12
		 (Op_Inspecc
	)

happyReduce_27 = happySpecReduce_1  9# happyReduction_27
happyReduction_27 happy_x_1
	 =  case happyOut14 happy_x_1 of { happy_var_1 -> 
	happyIn13
		 ([happy_var_1]
	)}

happyReduce_28 = happySpecReduce_3  9# happyReduction_28
happyReduction_28 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut14 happy_x_1 of { happy_var_1 -> 
	case happyOut13 happy_x_3 of { happy_var_3 -> 
	happyIn13
		 (happy_var_1 : happy_var_3
	)}}

happyReduce_29 = happySpecReduce_3  10# happyReduction_29
happyReduction_29 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOutTok happy_x_1 of { (TkIdent _ happy_var_1) -> 
	case happyOut16 happy_x_3 of { happy_var_3 -> 
	happyIn14
		 (I_Assign happy_var_1 happy_var_3
	)}}

happyReduce_30 = happyReduce 5# 10# happyReduction_30
happyReduction_30 (happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut15 happy_x_2 of { happy_var_2 -> 
	case happyOut13 happy_x_4 of { happy_var_4 -> 
	happyIn14
		 (I_If happy_var_2 happy_var_4
	) `HappyStk` happyRest}}

happyReduce_31 = happyReduce 7# 10# happyReduction_31
happyReduction_31 (happy_x_7 `HappyStk`
	happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut15 happy_x_2 of { happy_var_2 -> 
	case happyOut13 happy_x_4 of { happy_var_4 -> 
	case happyOut13 happy_x_6 of { happy_var_6 -> 
	happyIn14
		 (I_IfElse happy_var_2 happy_var_4 happy_var_6
	) `HappyStk` happyRest}}}

happyReduce_32 = happyReduce 5# 10# happyReduction_32
happyReduction_32 (happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut15 happy_x_2 of { happy_var_2 -> 
	case happyOut13 happy_x_4 of { happy_var_4 -> 
	happyIn14
		 (I_While happy_var_2 happy_var_4
	) `HappyStk` happyRest}}

happyReduce_33 = happyReduce 9# 10# happyReduction_33
happyReduction_33 (happy_x_9 `HappyStk`
	happy_x_8 `HappyStk`
	happy_x_7 `HappyStk`
	happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOutTok happy_x_2 of { (TkIdent _ happy_var_2) -> 
	case happyOut16 happy_x_4 of { happy_var_4 -> 
	case happyOut16 happy_x_6 of { happy_var_6 -> 
	case happyOut13 happy_x_8 of { happy_var_8 -> 
	happyIn14
		 (I_For happy_var_2 happy_var_4 happy_var_6 happy_var_8
	) `HappyStk` happyRest}}}}

happyReduce_34 = happyReduce 7# 10# happyReduction_34
happyReduction_34 (happy_x_7 `HappyStk`
	happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut16 happy_x_2 of { happy_var_2 -> 
	case happyOut16 happy_x_4 of { happy_var_4 -> 
	case happyOut13 happy_x_6 of { happy_var_6 -> 
	happyIn14
		 (I_From happy_var_2 happy_var_4 happy_var_6
	) `HappyStk` happyRest}}}

happyReduce_35 = happyReduce 5# 10# happyReduction_35
happyReduction_35 (happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut5 happy_x_2 of { happy_var_2 -> 
	case happyOut13 happy_x_4 of { happy_var_4 -> 
	happyIn14
		 (I_Declare happy_var_2 happy_var_4
	) `HappyStk` happyRest}}

happyReduce_36 = happySpecReduce_3  10# happyReduction_36
happyReduction_36 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut13 happy_x_2 of { happy_var_2 -> 
	happyIn14
		 (I_Declare [] happy_var_2
	)}

happyReduce_37 = happySpecReduce_2  10# happyReduction_37
happyReduction_37 happy_x_2
	happy_x_1
	 =  case happyOut16 happy_x_2 of { happy_var_2 -> 
	happyIn14
		 (I_Write happy_var_2
	)}

happyReduce_38 = happySpecReduce_2  10# happyReduction_38
happyReduction_38 happy_x_2
	happy_x_1
	 =  case happyOutTok happy_x_2 of { (TkIdent _ happy_var_2) -> 
	happyIn14
		 (I_Read happy_var_2
	)}

happyReduce_39 = happyReduce 5# 10# happyReduction_39
happyReduction_39 (happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut19 happy_x_2 of { happy_var_2 -> 
	case happyOut16 happy_x_5 of { happy_var_5 -> 
	happyIn14
		 (I_Ejec happy_var_2 happy_var_5
	) `HappyStk` happyRest}}

happyReduce_40 = happySpecReduce_3  10# happyReduction_40
happyReduction_40 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut16 happy_x_1 of { happy_var_1 -> 
	case happyOut16 happy_x_3 of { happy_var_3 -> 
	happyIn14
		 (I_Concat happy_var_1 happy_var_3
	)}}

happyReduce_41 = happySpecReduce_3  11# happyReduction_41
happyReduction_41 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut16 happy_x_1 of { happy_var_1 -> 
	case happyOut9 happy_x_2 of { happy_var_2 -> 
	case happyOut16 happy_x_3 of { happy_var_3 -> 
	happyIn15
		 (E_Comp happy_var_2 happy_var_1 happy_var_3
	)}}}

happyReduce_42 = happySpecReduce_1  11# happyReduction_42
happyReduction_42 happy_x_1
	 =  case happyOut16 happy_x_1 of { happy_var_1 -> 
	happyIn15
		 (happy_var_1
	)}

happyReduce_43 = happySpecReduce_3  12# happyReduction_43
happyReduction_43 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut16 happy_x_1 of { happy_var_1 -> 
	case happyOut10 happy_x_2 of { happy_var_2 -> 
	case happyOut17 happy_x_3 of { happy_var_3 -> 
	happyIn16
		 (E_BinOp happy_var_2 happy_var_1 happy_var_3
	)}}}

happyReduce_44 = happySpecReduce_1  12# happyReduction_44
happyReduction_44 happy_x_1
	 =  case happyOut17 happy_x_1 of { happy_var_1 -> 
	happyIn16
		 (happy_var_1
	)}

happyReduce_45 = happySpecReduce_3  13# happyReduction_45
happyReduction_45 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut17 happy_x_1 of { happy_var_1 -> 
	case happyOut11 happy_x_2 of { happy_var_2 -> 
	case happyOut18 happy_x_3 of { happy_var_3 -> 
	happyIn17
		 (E_BinOp happy_var_2 happy_var_1 happy_var_3
	)}}}

happyReduce_46 = happySpecReduce_1  13# happyReduction_46
happyReduction_46 happy_x_1
	 =  case happyOut18 happy_x_1 of { happy_var_1 -> 
	happyIn17
		 (happy_var_1
	)}

happyReduce_47 = happySpecReduce_1  14# happyReduction_47
happyReduction_47 happy_x_1
	 =  case happyOutTok happy_x_1 of { (TkIdent _ happy_var_1) -> 
	happyIn18
		 (E_Var happy_var_1
	)}

happyReduce_48 = happySpecReduce_1  14# happyReduction_48
happyReduction_48 happy_x_1
	 =  case happyOutTok happy_x_1 of { (TkNum _ happy_var_1) -> 
	happyIn18
		 (E_Const happy_var_1
	)}

happyReduce_49 = happySpecReduce_2  14# happyReduction_49
happyReduction_49 happy_x_2
	happy_x_1
	 =  case happyOut12 happy_x_1 of { happy_var_1 -> 
	case happyOut18 happy_x_2 of { happy_var_2 -> 
	happyIn18
		 (E_UnOp happy_var_1 happy_var_2
	)}}

happyReduce_50 = happySpecReduce_1  14# happyReduction_50
happyReduction_50 happy_x_1
	 =  happyIn18
		 (E_True
	)

happyReduce_51 = happySpecReduce_1  14# happyReduction_51
happyReduction_51 happy_x_1
	 =  happyIn18
		 (E_False
	)

happyReduce_52 = happySpecReduce_3  14# happyReduction_52
happyReduction_52 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut16 happy_x_2 of { happy_var_2 -> 
	happyIn18
		 (E_Corch happy_var_2
	)}

happyReduce_53 = happySpecReduce_3  14# happyReduction_53
happyReduction_53 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut16 happy_x_2 of { happy_var_2 -> 
	happyIn18
		 (E_Paren happy_var_2
	)}

happyReduce_54 = happySpecReduce_1  15# happyReduction_54
happyReduction_54 happy_x_1
	 =  case happyOut20 happy_x_1 of { happy_var_1 -> 
	happyIn19
		 ([happy_var_1]
	)}

happyReduce_55 = happySpecReduce_2  15# happyReduction_55
happyReduction_55 happy_x_2
	happy_x_1
	 =  case happyOut20 happy_x_1 of { happy_var_1 -> 
	case happyOut19 happy_x_2 of { happy_var_2 -> 
	happyIn19
		 (happy_var_1 : happy_var_2
	)}}

happyReduce_56 = happySpecReduce_1  16# happyReduction_56
happyReduction_56 happy_x_1
	 =  happyIn20
		 (C_Sum
	)

happyReduce_57 = happySpecReduce_1  16# happyReduction_57
happyReduction_57 happy_x_1
	 =  happyIn20
		 (C_Res
	)

happyReduce_58 = happySpecReduce_1  16# happyReduction_58
happyReduction_58 happy_x_1
	 =  happyIn20
		 (C_Izq
	)

happyReduce_59 = happySpecReduce_1  16# happyReduction_59
happyReduction_59 happy_x_1
	 =  happyIn20
		 (C_Der
	)

happyReduce_60 = happySpecReduce_1  16# happyReduction_60
happyReduction_60 happy_x_1
	 =  happyIn20
		 (C_Imp
	)

happyReduce_61 = happySpecReduce_1  16# happyReduction_61
happyReduction_61 happy_x_1
	 =  happyIn20
		 (C_Lee
	)

happyNewToken action sts stk [] =
	happyDoAction 49# notHappyAtAll action sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = happyDoAction i tk action sts stk tks in
	case tk of {
	TkIdent _ happy_dollar_dollar -> cont 1#;
	TkNum _ happy_dollar_dollar -> cont 2#;
	TkDeclare _ -> cont 3#;
	TkExecute _ -> cont 4#;
	TkDone _ -> cont 5#;
	TkWhile _ -> cont 6#;
	TkDo _ -> cont 7#;
	TkFor _ -> cont 8#;
	TkFrom _ -> cont 9#;
	TkTo _ -> cont 10#;
	TkIf _ -> cont 11#;
	TkThen _ -> cont 12#;
	TkElse _ -> cont 13#;
	TkRead _ -> cont 14#;
	TkWrite _ -> cont 15#;
	TkBoolean _ -> cont 16#;
	TkInteger _ -> cont 17#;
	TkTape _ -> cont 18#;
	TkTrue _ -> cont 19#;
	TkFalse _ -> cont 20#;
	TkComa _ -> cont 21#;
	TkPunto _ -> cont 22#;
	TkPuntoYComa _ -> cont 23#;
	TkParAbre _ -> cont 24#;
	TkParCierra _ -> cont 25#;
	TkType _ -> cont 26#;
	TkMas _ -> cont 27#;
	TkMenos _ -> cont 28#;
	TkMult _ -> cont 29#;
	TkDiv _ -> cont 30#;
	TkMod _ -> cont 31#;
	TkConjuncion _ -> cont 32#;
	TkDisyuncion _ -> cont 33#;
	TkNegacion _ -> cont 34#;
	TkMenor _ -> cont 35#;
	TkMenorIgual _ -> cont 36#;
	TkMayor _ -> cont 37#;
	TkMayorIgual _ -> cont 38#;
	TkIgual _ -> cont 39#;
	TkDesigual _ -> cont 40#;
	TkCorcheteAbre _ -> cont 41#;
	TkCorcheteCierra _ -> cont 42#;
	TkLlaveAbre _ -> cont 43#;
	TkLlaveCierra _ -> cont 44#;
	TkAt _ -> cont 45#;
	TkConcat _ -> cont 46#;
	TkInspeccion _ -> cont 47#;
	TkAsignacion _ -> cont 48#;
	_ -> happyError' (tk:tks)
	}

happyError_ tk tks = happyError' (tk:tks)

newtype HappyIdentity a = HappyIdentity a
happyIdentity = HappyIdentity
happyRunIdentity (HappyIdentity a) = a

instance Monad HappyIdentity where
    return = HappyIdentity
    (HappyIdentity p) >>= q = q p

happyThen :: () => HappyIdentity a -> (a -> HappyIdentity b) -> HappyIdentity b
happyThen = (>>=)
happyReturn :: () => a -> HappyIdentity a
happyReturn = (return)
happyThen1 m k tks = (>>=) m (\a -> k a tks)
happyReturn1 :: () => a -> b -> HappyIdentity a
happyReturn1 = \a tks -> (return) a
happyError' :: () => [(Token)] -> HappyIdentity a
happyError' = HappyIdentity . parseError

calc tks = happyRunIdentity happySomeParser where
  happySomeParser = happyThen (happyParse 0# tks) (\x -> happyReturn (happyOut4 x))

happySeq = happyDontSeq


--
-- Tipos de datos que representan un programa en Brainiac
--

type VarName = String
type Valor   = Int

data Declaracion = Decl VarName Tipo deriving (Show)

data Inst = I_Assign VarName Exp
          | I_If Exp [Inst] 
          | I_IfElse Exp [Inst] [Inst]
          | I_While Exp [Inst]
          | I_For VarName Exp Exp [Inst]
          | I_From Exp Exp [Inst]
          | I_Declare [Declaracion] [Inst]
          | I_Write Exp
          | I_Read VarName
          | I_Ejec [B_Inst] Exp
          | I_Concat Exp Exp

instance Show Inst where show = correrImpresor . impresor 

data Exp = E_Const Valor 
         | E_Var VarName 
         | E_True 
         | E_False 
         | E_BinOp OpBin Exp Exp 
         | E_Comp OpComp Exp Exp 
         | E_UnOp OpUn Exp 
         | E_Paren Exp 
         | E_Corch Exp

instance Show Exp where show = correrImpresor . impresorE

data B_Inst = C_Sum
            | C_Res
            | C_Izq
            | C_Der
            | C_Imp
            | C_Lee

instance Show B_Inst where
    show (C_Sum) = "+"
    show (C_Res) = "-"
    show (C_Izq) = "<"
    show (C_Der) = ">"
    show (C_Imp) = "."
    show (C_Lee) = ","

data OpBin = Op_Sum 
           | Op_Res
           | Op_Mul 
           | Op_Div 
           | Op_Mod
           | Op_Con
           | Op_Dis

instance Show OpBin where
    show Op_Sum = "'Suma'"
    show Op_Res = "'Resta'"
    show Op_Mul = "'Multiplicacion'"
    show Op_Div = "'Division'"
    show Op_Mod = "'Modulo'"
    show Op_Dis = "'Disyuncion'"
    show Op_Con = "'Conjuncion'"

data OpComp = Op_Eq
            | Op_Neq
            | Op_Lt 
            | Op_Leq 
            | Op_Gt
            | Op_Geq 

instance Show OpComp where
    show Op_Eq  = "'Igual'"
    show Op_Neq = "'No Igual'"
    show Op_Lt  = "'Menor que'"
    show Op_Leq = "'Menor o igual'"
    show Op_Gt  = "'Mayor que'"
    show Op_Geq = "'Mayor o igual'"

data OpUn = Op_NegArit
          | Op_NegBool
          | Op_Inspecc

instance Show OpUn where
    show Op_NegArit = "'Negacion Aritmetica'"
    show Op_NegBool = "'Negacion Booleana'"
    show Op_Inspecc = "'Inspeccion'"

data Tipo = Tipo_Boolean
          | Tipo_Integer 
          | Tipo_Tape
          deriving (Eq)

instance Show Tipo where
    show (Tipo_Integer) = "tipo entero"
    show (Tipo_Boolean) = "tipo boolean"
    show (Tipo_Tape)    = "tipo cinta"

--
-- Funcion de error
--
parseError :: [Token] -> a
parseError tks = error $ "Error sintactico, Simbolo inesperado " ++ show (head tks)

--
-- Impresion del AST (arbol sintactico abstracto)
--

data PrintState = PrintState {
    tabs :: Int
} deriving (Show)

initialPState :: PrintState
initialPState = PrintState {
    tabs = 0
}

type AST_String = DS.Seq String

correrImpresor :: Impresor () -> String
correrImpresor = (DF.foldl (++) "") . snd . runIdentity . runWriterT . (flip runStateT initialPState)

type Impresor a = StateT PrintState (WriterT AST_String Identity) a

impresor :: Inst -> Impresor ()

impresor (I_Assign id e) = do
    imprimirNoTerminal "ASIGNACION" 
    subirTabs
    imprimirNoTerminal $ "- variable: " ++ id
    imprimirExpresion    "- val: " e
    bajarTabs
impresor (I_If b exito) = do
    imprimirNoTerminal "CONDICIONAL" 
    subirTabs
    imprimirExpresion     "- guardia:" b
    imprimirInstrucciones "- exito: "  exito
    bajarTabs
impresor (I_IfElse b exito fallo) = do
    imprimirNoTerminal "CONDICIONAL IF-ELSE"
    subirTabs
    imprimirExpresion     "- guardia: " b
    imprimirInstrucciones "- exito: "   exito
    imprimirInstrucciones "- fallo: "   fallo
    bajarTabs
impresor (I_While b c) = do
    imprimirNoTerminal "ITERACION INDETERMINADA" 
    subirTabs
    imprimirExpresion     "- guardia:" b
    imprimirInstrucciones "- cuerpo:"  c
    bajarTabs
impresor (I_For id e1 e2 c) = do
    imprimirNoTerminal "ITERACION DETERMINADA - FOR" 
    subirTabs
    imprimirNoTerminal $  "- variable: " ++ id
    imprimirExpresion     "- e1: "     e1
    imprimirExpresion     "- e2: " e2
    imprimirInstrucciones "- cuerpo: " c
    bajarTabs
impresor (I_From e1 e2 c) = do
    imprimirNoTerminal "ITERACION DETERMINADA - FROM"
    subirTabs
    imprimirExpresion     "- lim_inferior:" e1 
    imprimirExpresion     "- lim_superior:" e2 
    imprimirInstrucciones "- cuerpo: "      c
    bajarTabs
impresor (I_Declare _ is) = imprimirInstrucciones "SECUENCIACION" is
impresor (I_Write e) = do
    imprimirNoTerminal "IMPRIMIR"
    subirTabs
    imprimirExpresion "- expr: " e
    bajarTabs
impresor (I_Read id) = do
    imprimirNoTerminal "LEER"
    subirTabs
    imprimirNoTerminal $ "- variable: " ++ id
    bajarTabs
impresor (I_Ejec cadena e) = do
    imprimirNoTerminal "EJECUCION"
    subirTabs
    imprimirCadena cadena
    imprimirExpresion "- cinta: " e
    bajarTabs
impresor (I_Concat e1 e2) = do
    imprimirNoTerminal "CONCATENACION"
    subirTabs
    imprimirExpresion "- 1era cadena: " e1
    imprimirExpresion "- 2da cadena: "  e2
    bajarTabs

--
--  Impresion de expresiones
--

impresorE :: Exp -> Impresor ()

impresorE (E_Const c)        = imprimirNoTerminal $ show c
impresorE (E_Var v)          = imprimirNoTerminal v
impresorE (E_True)           = imprimirNoTerminal "'True'"
impresorE (E_False)          = imprimirNoTerminal "'False'"
impresorE (E_BinOp op e1 e2) = do
    imprimirNoTerminal "BIN_ARITMETICO"
    subirTabs
    imprimirNoTerminal $ "- operacion: " ++ (show op)
    imprimirExpresion    "- operador izquierdo: " e1
    imprimirExpresion    "- operador derecho: "   e2
    bajarTabs
impresorE (E_Comp op e1 e2)  = do
    imprimirNoTerminal "BIN_RELACIONAL"
    subirTabs
    imprimirNoTerminal $ "- operacion: " ++ (show op)
    imprimirExpresion    "- operador izquierdo: " e1
    imprimirExpresion    "- operador derecho: " e2
    bajarTabs
impresorE (E_UnOp op e) = do
    imprimirExpresion (show op) e
impresorE (E_Paren e) = do
    imprimirNoTerminal "PARENTESIS" 
    subirTabs
    imprimirExpresion "- expr: " e
    bajarTabs
impresorE (E_Corch e) = do
    imprimirNoTerminal "CORCHETES" 
    subirTabs
    imprimirExpresion "- expr: " e
    bajarTabs

--
--  Funciones auxiliares para la impresion
--

subirTabs :: Impresor ()
subirTabs = modify (\s -> s { tabs = (tabs s) + 1 })

bajarTabs :: Impresor ()
bajarTabs = modify (\s -> s { tabs = (tabs s) - 1 })

imprimirNoTerminal :: String -> Impresor ()
imprimirNoTerminal str = do
    t <- gets tabs
    tell $ DS.singleton $ replicate t '\t' ++ str ++ "\n"

imprimirExpresion :: String -> Exp -> Impresor () 
imprimirExpresion tag e = do
    imprimirNoTerminal tag
    subirTabs
    impresorE e 
    bajarTabs

imprimirCadena :: [B_Inst] -> Impresor ()
imprimirCadena c = do
    imprimirNoTerminal "- cadena"
    subirTabs
    imprimirNoTerminal $ "{" ++ (concatMap show c) ++ "}" 
    bajarTabs

imprimirInstrucciones :: String -> [Inst] -> Impresor ()
imprimirInstrucciones tag is = do
    imprimirNoTerminal tag
    subirTabs
    mapM_ impresor is
    bajarTabs
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "<built-in>" #-}
{-# LINE 1 "<command-line>" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
-- Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp 

{-# LINE 28 "templates/GenericTemplate.hs" #-}


data Happy_IntList = HappyCons Happy_GHC_Exts.Int# Happy_IntList





{-# LINE 49 "templates/GenericTemplate.hs" #-}

{-# LINE 59 "templates/GenericTemplate.hs" #-}

{-# LINE 68 "templates/GenericTemplate.hs" #-}

infixr 9 `HappyStk`
data HappyStk a = HappyStk a (HappyStk a)

-----------------------------------------------------------------------------
-- starting the parse

happyParse start_state = happyNewToken start_state notHappyAtAll notHappyAtAll

-----------------------------------------------------------------------------
-- Accepting the parse

-- If the current token is 0#, it means we've just accepted a partial
-- parse (a %partial parser).  We must ignore the saved token on the top of
-- the stack in this case.
happyAccept 0# tk st sts (_ `HappyStk` ans `HappyStk` _) =
	happyReturn1 ans
happyAccept j tk st sts (HappyStk ans _) = 
	(happyTcHack j (happyTcHack st)) (happyReturn1 ans)

-----------------------------------------------------------------------------
-- Arrays only: do the next action



happyDoAction i tk st
	= {- nothing -}


	  case action of
		0#		  -> {- nothing -}
				     happyFail i tk st
		-1# 	  -> {- nothing -}
				     happyAccept i tk st
		n | (n Happy_GHC_Exts.<# (0# :: Happy_GHC_Exts.Int#)) -> {- nothing -}

				     (happyReduceArr Happy_Data_Array.! rule) i tk st
				     where rule = (Happy_GHC_Exts.I# ((Happy_GHC_Exts.negateInt# ((n Happy_GHC_Exts.+# (1# :: Happy_GHC_Exts.Int#))))))
		n		  -> {- nothing -}


				     happyShift new_state i tk st
				     where new_state = (n Happy_GHC_Exts.-# (1# :: Happy_GHC_Exts.Int#))
   where off    = indexShortOffAddr happyActOffsets st
	 off_i  = (off Happy_GHC_Exts.+# i)
	 check  = if (off_i Happy_GHC_Exts.>=# (0# :: Happy_GHC_Exts.Int#))
			then (indexShortOffAddr happyCheck off_i Happy_GHC_Exts.==#  i)
			else False
 	 action | check     = indexShortOffAddr happyTable off_i
		| otherwise = indexShortOffAddr happyDefActions st

{-# LINE 127 "templates/GenericTemplate.hs" #-}


indexShortOffAddr (HappyA# arr) off =
#if __GLASGOW_HASKELL__ > 500
	Happy_GHC_Exts.narrow16Int# i
#elif __GLASGOW_HASKELL__ == 500
	Happy_GHC_Exts.intToInt16# i
#else
	Happy_GHC_Exts.iShiftRA# (Happy_GHC_Exts.iShiftL# i 16#) 16#
#endif
  where
#if __GLASGOW_HASKELL__ >= 503
	i = Happy_GHC_Exts.word2Int# (Happy_GHC_Exts.or# (Happy_GHC_Exts.uncheckedShiftL# high 8#) low)
#else
	i = Happy_GHC_Exts.word2Int# (Happy_GHC_Exts.or# (Happy_GHC_Exts.shiftL# high 8#) low)
#endif
	high = Happy_GHC_Exts.int2Word# (Happy_GHC_Exts.ord# (Happy_GHC_Exts.indexCharOffAddr# arr (off' Happy_GHC_Exts.+# 1#)))
	low  = Happy_GHC_Exts.int2Word# (Happy_GHC_Exts.ord# (Happy_GHC_Exts.indexCharOffAddr# arr off'))
	off' = off Happy_GHC_Exts.*# 2#





data HappyAddr = HappyA# Happy_GHC_Exts.Addr#




-----------------------------------------------------------------------------
-- HappyState data type (not arrays)

{-# LINE 170 "templates/GenericTemplate.hs" #-}

-----------------------------------------------------------------------------
-- Shifting a token

happyShift new_state 0# tk st sts stk@(x `HappyStk` _) =
     let i = (case Happy_GHC_Exts.unsafeCoerce# x of { (Happy_GHC_Exts.I# (i)) -> i }) in
--     trace "shifting the error token" $
     happyDoAction i tk new_state (HappyCons (st) (sts)) (stk)

happyShift new_state i tk st sts stk =
     happyNewToken new_state (HappyCons (st) (sts)) ((happyInTok (tk))`HappyStk`stk)

-- happyReduce is specialised for the common cases.

happySpecReduce_0 i fn 0# tk st sts stk
     = happyFail 0# tk st sts stk
happySpecReduce_0 nt fn j tk st@((action)) sts stk
     = happyGoto nt j tk st (HappyCons (st) (sts)) (fn `HappyStk` stk)

happySpecReduce_1 i fn 0# tk st sts stk
     = happyFail 0# tk st sts stk
happySpecReduce_1 nt fn j tk _ sts@((HappyCons (st@(action)) (_))) (v1`HappyStk`stk')
     = let r = fn v1 in
       happySeq r (happyGoto nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_2 i fn 0# tk st sts stk
     = happyFail 0# tk st sts stk
happySpecReduce_2 nt fn j tk _ (HappyCons (_) (sts@((HappyCons (st@(action)) (_))))) (v1`HappyStk`v2`HappyStk`stk')
     = let r = fn v1 v2 in
       happySeq r (happyGoto nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_3 i fn 0# tk st sts stk
     = happyFail 0# tk st sts stk
happySpecReduce_3 nt fn j tk _ (HappyCons (_) ((HappyCons (_) (sts@((HappyCons (st@(action)) (_))))))) (v1`HappyStk`v2`HappyStk`v3`HappyStk`stk')
     = let r = fn v1 v2 v3 in
       happySeq r (happyGoto nt j tk st sts (r `HappyStk` stk'))

happyReduce k i fn 0# tk st sts stk
     = happyFail 0# tk st sts stk
happyReduce k nt fn j tk st sts stk
     = case happyDrop (k Happy_GHC_Exts.-# (1# :: Happy_GHC_Exts.Int#)) sts of
	 sts1@((HappyCons (st1@(action)) (_))) ->
        	let r = fn stk in  -- it doesn't hurt to always seq here...
       		happyDoSeq r (happyGoto nt j tk st1 sts1 r)

happyMonadReduce k nt fn 0# tk st sts stk
     = happyFail 0# tk st sts stk
happyMonadReduce k nt fn j tk st sts stk =
        happyThen1 (fn stk tk) (\r -> happyGoto nt j tk st1 sts1 (r `HappyStk` drop_stk))
       where sts1@((HappyCons (st1@(action)) (_))) = happyDrop k (HappyCons (st) (sts))
             drop_stk = happyDropStk k stk

happyMonad2Reduce k nt fn 0# tk st sts stk
     = happyFail 0# tk st sts stk
happyMonad2Reduce k nt fn j tk st sts stk =
       happyThen1 (fn stk tk) (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))
       where sts1@((HappyCons (st1@(action)) (_))) = happyDrop k (HappyCons (st) (sts))
             drop_stk = happyDropStk k stk

             off    = indexShortOffAddr happyGotoOffsets st1
             off_i  = (off Happy_GHC_Exts.+# nt)
             new_state = indexShortOffAddr happyTable off_i




happyDrop 0# l = l
happyDrop n (HappyCons (_) (t)) = happyDrop (n Happy_GHC_Exts.-# (1# :: Happy_GHC_Exts.Int#)) t

happyDropStk 0# l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n Happy_GHC_Exts.-# (1#::Happy_GHC_Exts.Int#)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction


happyGoto nt j tk st = 
   {- nothing -}
   happyDoAction j tk new_state
   where off    = indexShortOffAddr happyGotoOffsets st
	 off_i  = (off Happy_GHC_Exts.+# nt)
 	 new_state = indexShortOffAddr happyTable off_i




-----------------------------------------------------------------------------
-- Error recovery (0# is the error token)

-- parse error if we are in recovery and we fail again
happyFail  0# tk old_st _ stk =
--	trace "failing" $ 
    	happyError_ tk

{-  We don't need state discarding for our restricted implementation of
    "error".  In fact, it can cause some bogus parses, so I've disabled it
    for now --SDM

-- discard a state
happyFail  0# tk old_st (HappyCons ((action)) (sts)) 
						(saved_tok `HappyStk` _ `HappyStk` stk) =
--	trace ("discarding state, depth " ++ show (length stk))  $
	happyDoAction 0# tk action sts ((saved_tok`HappyStk`stk))
-}

-- Enter error recovery: generate an error token,
--                       save the old token and carry on.
happyFail  i tk (action) sts stk =
--      trace "entering error recovery" $
	happyDoAction 0# tk action sts ( (Happy_GHC_Exts.unsafeCoerce# (Happy_GHC_Exts.I# (i))) `HappyStk` stk)

-- Internal happy errors:

notHappyAtAll = error "Internal Happy error\n"

-----------------------------------------------------------------------------
-- Hack to get the typechecker to accept our action functions


happyTcHack :: Happy_GHC_Exts.Int# -> a -> a
happyTcHack x y = y
{-# INLINE happyTcHack #-}


-----------------------------------------------------------------------------
-- Seq-ing.  If the --strict flag is given, then Happy emits 
--	happySeq = happyDoSeq
-- otherwise it emits
-- 	happySeq = happyDontSeq

happyDoSeq, happyDontSeq :: a -> b -> b
happyDoSeq   a b = a `seq` b
happyDontSeq a b = b

-----------------------------------------------------------------------------
-- Don't inline any functions from the template.  GHC has a nasty habit
-- of deciding to inline happyGoto everywhere, which increases the size of
-- the generated parser quite a bit.


{-# NOINLINE happyDoAction #-}
{-# NOINLINE happyTable #-}
{-# NOINLINE happyCheck #-}
{-# NOINLINE happyActOffsets #-}
{-# NOINLINE happyGotoOffsets #-}
{-# NOINLINE happyDefActions #-}

{-# NOINLINE happyShift #-}
{-# NOINLINE happySpecReduce_0 #-}
{-# NOINLINE happySpecReduce_1 #-}
{-# NOINLINE happySpecReduce_2 #-}
{-# NOINLINE happySpecReduce_3 #-}
{-# NOINLINE happyReduce #-}
{-# NOINLINE happyMonadReduce #-}
{-# NOINLINE happyGoto #-}
{-# NOINLINE happyFail #-}

-- end of Happy Template.
