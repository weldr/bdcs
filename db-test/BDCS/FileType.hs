{-# LANGUAGE TemplateHaskell #-}

module BDCS.FileType where

import Data.Bits((.&.))
import Database.Persist.TH

data FileType = RegularFile | Directory | Socket | SymbolicLink | BlockDevice | CharDevice | FIFO
 deriving(Eq, Read, Show)

derivePersistField "FileType"

-- FIXME:  Really need to figure out a library that has this
getFileType :: Int -> FileType
getFileType mode = case mode .&. 0o170000 of
    0o140000 -> Socket
    0o120000 -> SymbolicLink
    0o100000 -> RegularFile
    0o060000 -> BlockDevice
    0o040000 -> Directory
    0o020000 -> CharDevice
    0o010000 -> FIFO
    _        -> RegularFile
