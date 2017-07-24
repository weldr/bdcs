-- Copyright (C) 2017 Red Hat, Inc.
--
-- This library is free software; you can redistribute it and/or
-- modify it under the terms of the GNU Lesser General Public
-- License as published by the Free Software Foundation; either
-- version 2.1 of the License, or (at your option) any later version.
--
-- This library is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
-- Lesser General Public License for more details.
--
-- You should have received a copy of the GNU Lesser General Public
-- License along with this library; if not, see <http://www.gnu.org/licenses/>.
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts  #-}

-- | Parse a tmpfiles.d config file into TmpFileEntry records.
--
-- This parser is somewhat limited, it only supports types that are needed
-- by the bdcs export tool (eg. creating files and directories)
--
module Export.TmpFiles(
    TmpFileEntry(..),
    TmpFileType(..),
    parseConfString)
 where

import qualified Data.Text as T
import           Text.Parsec
import           Text.ParserCombinators.Parsec.Char(CharParser)
import           Text.ParserCombinators.Parsec.Number(number)

-- Types for the tmpfiles.d config file
-- This is not a complete list, some don't make sense for an empty filesystem and are unimplemented
-- NOTE Order is important, it needs to maintain at least: Directory, Symlink, File, etc.
data TmpFileType = NewDirectory
                 | ReplaceDirectory             -- This is conditional on --remove in systemd-tmpfiles
                 | NewSymlink
                 | ReplaceSymlink
                 | NewFile
                 | TruncateFile
                 | WriteFile
                 | ModifyDirectory
                 | NewCharDev
                 | ReplaceCharDev
                 | NewBlockDev
                 | ReplaceBlockDev
                 | Copy
                 | Unsupported                  -- Catchall for unsupported types
  deriving(Ord, Eq, Show)

-- Translate the type characters into the actual types
getTmpFileType :: String -> TmpFileType
getTmpFileType "f" = NewFile
getTmpFileType "F" = TruncateFile
getTmpFileType "w" = WriteFile
getTmpFileType "d" = NewDirectory
getTmpFileType "D" = ReplaceDirectory
getTmpFileType "e" = ModifyDirectory
getTmpFileType "L" = NewSymlink
getTmpFileType "L+"= ReplaceSymlink
getTmpFileType "c" = NewCharDev
getTmpFileType "c+"= ReplaceCharDev
getTmpFileType "b" = NewBlockDev
getTmpFileType "b+"= ReplaceBlockDev
getTmpFileType "C" = Copy
getTmpFileType _   = Unsupported

allowedTypes :: String
allowedTypes = "fFwdDevqQpLcbCxXrRzZtThHaA+!"

-- Record for the tmpfiles.d config file entries
data TmpFileEntry = TmpFileEntry {
    -- | The type of file to create
    tfeType :: TmpFileType,
    tfePath :: FilePath,
    tfeMode :: Maybe Integer,
    tfeUid  :: Maybe T.Text,
    tfeGid  :: Maybe T.Text,
    tfeAge  :: Maybe T.Text,
    tfeArg  :: Maybe T.Text }
  deriving(Eq, Show)

-- Order the records by: Directory, Symlink, File, etc. and when equal, sort by the path.
instance Ord TmpFileEntry where
    a `compare` b = let cmp = tfeType a `compare` tfeType b
                    in if cmp == EQ then tfePath a `compare` tfePath b else cmp

eol :: Parsec String () Char
eol = char '\n'

-- | Parse an Octal string with leading 0, o, O, or a
-- combination thereof.
octal :: Integral i => CharParser st i
octal = many1 (oneOf "Oo0") >> number 8 octDigit

-- | Skip spaces. NOT \n or \r. Also skip \t because they are ugly
skipSpaces :: Parsec String () ()
skipSpaces = skipMany (oneOf " \t")

-- | Get a String field and convert it to T.Text
getTextField :: Parsec String () T.Text
getTextField = T.pack <$> many (noneOf " ")

-- | Parse a field that might have a '-' into a Maybe, using the supplied function to parse the value
parseMaybeDash :: Stream s m Char => ParsecT s u m a -> ParsecT s u m (Maybe a)
parseMaybeDash f = (Nothing <$ char '-') <|> (return <$> f)

-- | Convert the field into a tmpfiles.d entry type
parseType :: Parsec String () TmpFileType
parseType = getTmpFileType <$> many1 (oneOf allowedTypes)

-- | Parse a file path
parsePath :: Parsec String () FilePath
parsePath = many1 (noneOf " ")

-- | Parse an Octal file mode
-- Which can also be set to '-' to mean the default for the type
parseMode :: Parsec String () (Maybe Integer)
parseMode = parseMaybeDash octal

-- | Parse a uid/gid (only supports strings)
parseId :: Parsec String () (Maybe T.Text)
parseId = parseMaybeDash getTextField

-- | Age may be the last entry, or it may not.
parseAge :: Parsec String () (Maybe T.Text)
parseAge = parseMaybeDash getAgeField
  where
    getAgeField = T.pack <$> many1 (oneOf "0123456789sminhdwu-")

-- | If Arg is present, it is the last, and may contain spaces
parseArg :: Parsec String () T.Text
parseArg = T.pack <$> many1 (noneOf "\n")

parseConfLine :: Parsec String () TmpFileEntry
parseConfLine = do
    t <- parseType
    skipSpaces
    p <- parsePath
    skipSpaces
    m <- parseMode
    skipSpaces
    uid <- parseId
    skipSpaces
    gid <- parseId
    skipSpaces
    age <- parseAge
    skipSpaces
    arg <- optionMaybe $ try parseArg
    eol

    return TmpFileEntry{tfeType=t, tfePath=p, tfeMode=m, tfeUid=uid, tfeGid=gid, tfeAge=age, tfeArg=arg}

parseConfString :: String -> Either ParseError [TmpFileEntry]
parseConfString = parse (many1 parseConfLine) "(tmpFiles.d)"
