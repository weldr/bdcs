{-# LANGUAGE TemplateHaskell #-}

module FileType where

import Database.Persist.TH

data FileType = RegularFile | Directory | Socket | SymbolicLink | BlockDevice | CharDevice | FIFO
 deriving(Eq, Read, Show)

derivePersistField "FileType"
