{-# LANGUAGE MultiWayIf #-}

module Utils.Mode(modeAsText)
 where

import           Data.Bits(testBit)
import qualified Data.Text as T
import           Data.Word(Word32)

modeAsText :: Word32 -> T.Text
modeAsText x = T.pack [
    if x `testBit` 8 then 'r' else '-',
    if x `testBit` 7 then 'w' else '-',
    if | x `testBit` 12 && x `testBit` 6 -> 's'
       | x `testBit` 12                  -> 'S'
       | x `testBit` 6                   -> 'x'
       | otherwise                       -> '-',
    if x `testBit` 5 then 'r' else '-',
    if x `testBit` 4 then 'w' else '-',
    if | x `testBit` 11 && x `testBit` 3 -> 's'
       | x `testBit` 11                  -> 'S'
       | x `testBit` 3                   -> 'x'
       | otherwise                       -> '-',
    if x `testBit` 2 then 'r' else '-',
    if x `testBit` 1 then 'w' else '-',
    if | x `testBit` 10 && x `testBit` 0 -> 't'
       | x `testBit` 10                  -> 'T'
       | x `testBit` 0                   -> 'x'
       | otherwise                       -> '-'
 ]
