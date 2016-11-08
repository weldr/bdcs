{-# LANGUAGE TemplateHaskell #-}

module BDCS.ReqType where

import Database.Persist.TH

data ReqLanguage = RPM
 deriving(Eq, Read, Show)

data ReqContext = Build | Runtime | Test
 deriving(Eq, Read, Show)

data ReqStrength = Must | Should | May
 deriving(Eq, Read, Show)

derivePersistField "ReqLanguage"
derivePersistField "ReqContext"
derivePersistField "ReqStrength"
