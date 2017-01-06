-- Copyright (C) 2016 Red Hat, Inc.
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

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes #-}

import           Conduit(MonadResource, awaitForever, runResourceT, sinkFile, sourceLazy, sourceFile)
import           Control.Monad(void, when)
import           Control.Monad.Except(MonadError, runExceptT)
import           Control.Monad.IO.Class(liftIO)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import           Data.CPIO(Entry(..), isEntryDirectory, readCPIO)
import           Data.Conduit(($$), (=$=), Conduit, Producer, yield)
import qualified Data.Conduit.Combinators as DCC
import           Data.Conduit.Lzma(decompress)
import           System.Directory(createDirectoryIfMissing)
import           System.Environment(getArgs)
import           System.Exit(exitFailure)
import           System.FilePath((</>), splitFileName)

import RPM.Parse(parseRPMC)
import RPM.Types(RPM(..))

-- Grab an RPM from stdin and convert it into a chunked conduit of ByteStrings.  This could
-- just as easily come from a stdin (using stdinC) or over the network (see httpSink in
-- the http-conduit package, for instance).
--
-- Important note:  stdinC produces a conduit of multiple ByteStrings.  A single ByteString
-- is some chunk of the file - I don't know how much, but it feels like maybe 32k.  conduit calls
-- this chunked data, and while chunks are more efficient for conduit to work with, they don't do
-- much for us.  To examine the RPM header, we need individual bytes.  Luckily, the *CE functions
-- out of conduit let us get at individual elements out of the stream of chunks without even
-- having to think about a chunk or asking for more.  That all happens for us.
--
-- Long story short, chunks are useful for when we need to just pipe lots of data from one place
-- to another or when we need to tell conduit about types.  Elements are useful for when we need
-- to do something to the contents.
getRPM :: MonadResource m => FilePath -> Producer m BS.ByteString
getRPM = sourceFile


-- If a cpio entry is a directory, just create it.  If it's a file, create the directory containing
-- it and then stream the contents onto disk.  I'm not worrying with permissions, file mode, timestamps,
-- or any of that stuff here.  This is just a demo.
writeCpioEntry :: Entry -> IO ()
writeCpioEntry entry@Entry{..} | isEntryDirectory entry = createDirectoryIfMissing True (BC.unpack cpioFileName)
                               | otherwise              = do
    let (d, f) = splitFileName (BC.unpack cpioFileName)
    createDirectoryIfMissing True d
    void $ runResourceT $ sourceLazy cpioFileData $$ sinkFile (d </> f)

-- Pull the rpmArchive out of the RPM record
payloadC :: MonadError e m => Conduit RPM m BS.ByteString
payloadC = awaitForever (yield . rpmArchive)

processRPM :: FilePath -> IO ()
processRPM path = do
    -- Hopefully self-explanatory - runErrorT and runResourceT execute and unwrap the monads, giving the
    -- actual result of the whole conduit.  That's either an error message nothing, and the files are
    -- written out in the pipeline kind of as a side effect.
    result <- runExceptT $ runResourceT $
            getRPM path
        =$= parseRPMC
        =$= payloadC
        =$= decompress Nothing
        =$= readCPIO
        $$  DCC.mapM_ (liftIO . writeCpioEntry)

    case result of
        Left e  -> print e
        Right _ -> return ()

main :: IO ()
main = do
    -- Read the list of rpms to process from the command line arguments
    argv <- getArgs

    when (length argv < 1) $ do
        putStrLn "Usage: unrpm RPM [RPM ...]"
        exitFailure

    mapM_ processRPM argv
