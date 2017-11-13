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
{-# LANGUAGE RecordWildCards   #-}

-- | Parse a tmpfiles.d config file into TmpFileEntry records.
--
-- This parser is limited, it only supports types that are needed
-- by the bdcs export tool (eg. creating files and directories)
--
-- Currently supported types are:
--
-- f    Create a new file and optionally write the arg to it. Will not overwrite.
-- F    Remove existing file and make a new one, optionally writing arg to it.
-- d    Create a new directory, only if it doesn't exist.
-- e    Modify an existing directory's ownership and permissions
-- L    Create a new symlink. Do nothing if it already exists.
-- L+   Remove file, directory tree, or symlink before creating it. WARNING this will remove a whole directory tree.
--
module Export.TmpFiles(
    TmpFileEntry(..),
    TmpFileType(..),
    parseConfString,
    setupFilesystem)
 where

import           Control.Conditional(ifM)
import           Data.List(sort)
import qualified Data.Text as T
import           System.Directory(createDirectoryIfMissing, doesPathExist, removePathForcibly)
import           System.FilePath((</>), dropDrive, takeFileName)
import           System.Posix.Files(createSymbolicLink, setFileMode, setOwnerAndGroup)
import           System.Posix.Types(CMode(..), CUid(..), CGid(..))
import           Text.Parsec
import           Text.ParserCombinators.Parsec.Char(CharParser)
import           Text.ParserCombinators.Parsec.Number(number)
import           Text.Printf(printf)

-- Types for the tmpfiles.d config file
-- This is not a complete list, some don't make sense for an empty filesystem and are unimplemented
-- NOTE Order is important, it needs to maintain at least: Directory, Symlink, File, etc.
data TmpFileType = NewDirectory
                 | NewSymlink
                 | ReplaceSymlink
                 | NewFile
                 | TruncateFile
                 | ModifyDirectory
                 | Unsupported                  -- Catchall for unsupported types
  deriving(Ord, Eq, Show)

-- Translate the type characters into the actual types
getTmpFileType :: String -> TmpFileType
getTmpFileType "f" = NewFile
getTmpFileType "F" = TruncateFile
getTmpFileType "d" = NewDirectory
getTmpFileType "e" = ModifyDirectory
getTmpFileType "L" = NewSymlink
getTmpFileType "L+"= ReplaceSymlink
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
-- Which can also be set to '-' to mean the default for the type
parseId :: Parsec String () (Maybe T.Text)
parseId = parseMaybeDash getTextField

-- | Age may be the last entry, or it may not.
-- It can also be set to '-' to mean the default for the type
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
    _ <- eol

    return TmpFileEntry{tfeType=t, tfePath=p, tfeMode=m, tfeUid=uid, tfeGid=gid, tfeAge=age, tfeArg=arg}

parseConfString :: String -> Either ParseError [TmpFileEntry]
parseConfString = parse (many1 parseConfLine) "(tmpFiles.d)"

-- TODO This is going to need a map from strings to ids
-- Use root for now
-- | Convert an owner name (eg. root) to a CUid value
owner :: Maybe T.Text -> CUid
owner uid = case uid of
    Nothing -> CUid 0
    Just _  -> CUid 0

-- TODO This is going to need a map from strings to ids
-- Use root for now
-- | Convert a group name (eg. root) to a CUid value
group :: Maybe T.Text -> CGid
group gid = case gid of
    Nothing -> CGid 0
    Just _  -> CGid 0


-- | Write a new file and set its ownership and permissions
writeNewFile :: FilePath -> TmpFileEntry -> IO ()
writeNewFile outPath TmpFileEntry{..} = do
    writeFile file content
    setFileMode file mode
    setOwnerAndGroup file (owner tfeUid) (group tfeGid)
  where
    file = outPath </> dropDrive tfePath
    content = case tfeArg of
        Nothing -> ""
        Just c  -> T.unpack c
    mode = case tfeMode of
        Nothing -> CMode 0o644
        Just m  -> CMode $ fromIntegral m

-- | Create a new directory if there isn't already one present
-- Also sets the ownership and permissions
applyEntry :: FilePath -> TmpFileEntry -> IO ()
applyEntry outPath TmpFileEntry{tfeType=NewDirectory, ..} = do
    createDirectoryIfMissing True dir
    setFileMode dir mode
    setOwnerAndGroup dir (owner tfeUid) (group tfeGid)
  where
    dir = outPath </> dropDrive tfePath
    mode = case tfeMode of
        Nothing -> CMode 0o755
        Just m  -> CMode $ fromIntegral m

-- | Create a new file with optional contents
-- Also sets the ownership and permissions
applyEntry outPath entry@TmpFileEntry{tfeType=NewFile, ..} =
    ifM (doesPathExist file)
        (printf "NewFile: %s already exists, skipping it." file)
        (writeNewFile outPath entry)
  where
    file = outPath </> dropDrive tfePath

-- | Create or Truncate a file with optional contents
-- Also sets the ownership and permissions
applyEntry outPath entry@TmpFileEntry{tfeType=TruncateFile, ..} = writeNewFile outPath entry

-- | Modify an existing directory's ownership and permissions
applyEntry outPath TmpFileEntry{tfeType=ModifyDirectory, ..} =
    ifM (doesPathExist dir)
        modify
        (printf "ModifyDirectory: %s doesn't exist, skipping it." dir)
  where
    dir = outPath </> dropDrive tfePath
    mode = case tfeMode of
        Nothing -> CMode 0o755
        Just m  -> CMode $ fromIntegral m
    modify = do
        setFileMode dir mode
        setOwnerAndGroup dir (owner tfeUid) (group tfeGid)


-- | Create a new symlink
-- Does NOT create parents of the source file, they must already exist
-- If no target arg is present it will link to the source filename under /usr/share/factory/
applyEntry outPath TmpFileEntry{tfeType=NewSymlink, ..} =
    ifM (doesPathExist source)
        (printf "NewSymlink: %s exists, skipping." source)
        (createSymbolicLink target source)
  where
    source = outPath </> dropDrive tfePath
    target = case tfeArg of
        Nothing  -> "/usr/share/factory" </> takeFileName tfePath
        Just arg -> T.unpack arg

-- | Replace a symlink, if it exists or create a new one
-- If no target arg is present it will link to the source filename under /usr/share/factory/
applyEntry outPath TmpFileEntry{tfeType=ReplaceSymlink, ..} = do
    removePathForcibly source
    createSymbolicLink target source
  where
    source = outPath </> dropDrive tfePath
    target = case tfeArg of
        Nothing  -> "/usr/share/factory" </> takeFileName tfePath
        Just arg -> T.unpack arg

applyEntry _ TmpFileEntry{tfeType=Unsupported, ..} = undefined


-- | Read the tmpfiles.d snippet and apply it to the output directory
setupFilesystem :: FilePath -> FilePath -> IO ()
setupFilesystem outPath tmpFileConf = do
    createDirectoryIfMissing True outPath
    tmpfiles <- parseConfString <$> readFile tmpFileConf
    case tmpfiles of
        Right entries -> mapM_ (applyEntry outPath) $ sort entries
        Left  err     -> print err
