{-# LANGUAGE TemplateHaskell       #-}

-- |
-- Module      : Data.Array.Accelerate.AST.ExpandFusionStrategy
-- Copyright   : [2008..2023] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- This module contains a DataType that is used to switch the fusion strategy of the PermutedExpand kernel
module Data.Array.Accelerate.AST.ExpandFusionStrategy
  where

import Language.Haskell.TH.Extra                                    ( CodeQ )
import Data.Array.Accelerate.Analysis.Hash.TH
import Data.ByteString.Builder
import Data.ByteString.Builder.Extra
import Data.Hashable (hashWithSalt)

data ExpandFusionStrategy 
    = Basic          -- | Most basic variant. Every thread handles 1 element of the input (expands and permutes it).
    | Blocks         -- | Slightly better than basic. Every thread block handles 1 element of the input. Every thread expands 1 element-index combination and permutes it. Every thread will read the input element to do this, thus every input element will be read multiple times.
    | Shuffle        -- | Every warp handles 1 element of the input. Every lane expands 1 element-index combination and permutes it. A value is read only once per warp.
    | BlockShuffle   -- | Every thread block handles 1 element of the input. This element is read only once per warp. Every thread/lane expands 1 element-index combination and permutes it.
    | SharedMem      -- | Every thread block handles 1 element of the input. The element is read only once per block and stored in shared memory. Every thread expands 1 element-index combination and permutes it.
    | MultiBlock Int -- | Every block handles multiple elements of the input, this should spread "random" inbalance. Within a block, the work is then spread over all threads.
    deriving Show

liftExpandStrategy :: ExpandFusionStrategy -> CodeQ ExpandFusionStrategy
liftExpandStrategy Basic     = [|| Basic ||]
liftExpandStrategy Blocks    = [|| Blocks ||]
liftExpandStrategy BlockShuffle = [|| BlockShuffle ||]
liftExpandStrategy Shuffle   = [|| Shuffle ||]
liftExpandStrategy SharedMem = [|| SharedMem ||]
liftExpandStrategy (MultiBlock i) = [|| MultiBlock i ||]

encodeExpandStrategy :: ExpandFusionStrategy -> Builder
encodeExpandStrategy Basic = intHost $(hashQ "Basic")
encodeExpandStrategy Blocks = intHost $(hashQ "Blocks")
encodeExpandStrategy BlockShuffle = intHost $(hashQ "BlockShuffle")
encodeExpandStrategy Shuffle = intHost $(hashQ "Shuffle")
encodeExpandStrategy SharedMem = intHost $(hashQ "SharedMem")
encodeExpandStrategy (MultiBlock i) = intHost $(hashQ "MultiBlock") <> intHost i