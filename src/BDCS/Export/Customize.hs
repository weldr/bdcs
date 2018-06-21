{-# LANGUAGE FlexibleContexts #-}

module BDCS.Export.Customize(CSOverlay,
                             Customization(..),
                             addToOverlay,
                             filesToObjectsC,
                             runCustomizations)
 where

import qualified Control.Exception.Lifted as CEL
import           Control.Monad(foldM)
import           Control.Monad.Except(MonadError, throwError)
import           Control.Monad.IO.Class(MonadIO)
import           Control.Monad.Logger(MonadLogger, MonadLoggerIO, logDebugN)
import           Control.Monad.Trans.Control(MonadBaseControl)
import           Crypto.Hash(Digest, hash)
import           Crypto.Hash.Algorithms(Blake2b_256)
import           Data.ByteArray(convert)
import qualified Data.ByteString as BS
import           Data.Conduit(Conduit, awaitForever, yield)
import           Data.ContentStore(ContentStore)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T

import BDCS.CS(Object(..), fileToObjectC)
import BDCS.DB(Files(..))
import BDCS.Export.FSTree(FSTree, addFileToTree)

type CSOverlay = Map.Map BS.ByteString Object

-- Just one type of customization for now, more to come later
{-# ANN module ("HLint: ignore Use newtype instead of data" :: String) #-}
data Customization = WriteFile Files (Maybe BS.ByteString)
 deriving (Eq, Show)

-- Not everything looks good as an operator, hlint
{-# ANN filesToObjectsC ("HLint: ignore Use section" :: String) #-}
filesToObjectsC :: (MonadError String m, MonadIO m) => CSOverlay -> ContentStore -> Conduit Files m (Files, Object)
filesToObjectsC overlay repo = awaitForever $ \f@Files{..} ->
    case maybe Nothing (flip Map.lookup overlay) filesCs_object of
        Nothing  -> fileToObjectC repo f
        Just obj -> yield (f, obj)

addToOverlay :: (MonadError String m, MonadLogger m) => CSOverlay -> FSTree -> Files -> Maybe BS.ByteString -> m (CSOverlay, FSTree)
addToOverlay overlay tree file content = do
    logDebugN $ T.pack "Adding to overlay: " `T.append` filesPath file

    -- If the file has content, create a hash of it and add the content to the overlay.
    -- The digest type doesn't need to match the content store, it just needs to be something we can
    -- use as a hash key.
    let (newFile, newOverlay) = case content of
            Nothing-> (file{filesCs_object = Nothing}, overlay)
            Just c -> let digest = makeDigest c
                      in (file{filesCs_object = Just digest}, Map.insert digest (FileObject c) overlay)

    -- Add the metadata to the FSTree
    newTree <- addFileToTree True tree newFile

    return (newOverlay, newTree)
 where
    makeDigest :: BS.ByteString -> BS.ByteString
    makeDigest input =
        let digest = hash input :: Digest Blake2b_256
        in  convert digest

runCustomizations :: (MonadBaseControl IO m, MonadError String m, MonadLoggerIO m) =>
                     CSOverlay
                  -> ContentStore
                  -> FSTree
                  -> [Customization]
                  -> m (CSOverlay, FSTree)
runCustomizations overlay _repo tree customizations = do
    logDebugN $ T.pack "Running customizations"
    foldM runCustomization (overlay, tree) customizations `CEL.catch` \e -> throwError $ show (e :: CEL.IOException)
 where
    runCustomization :: (MonadError String m, MonadLoggerIO m) => (CSOverlay, FSTree) -> Customization -> m (CSOverlay, FSTree)
    runCustomization (o, t) (WriteFile file content) = addToOverlay o t file content
