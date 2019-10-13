{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module State where

import System.IO.Error hiding (catch)
import Prelude hiding (catch)
import Control.Exception

import Data.Maybe
import Data.Time.LocalTime
import System.Info
import System.FilePath
import System.Directory
import GHC.Generics
import Data.Aeson (withText, Value(..), eitherDecode', encode, decode, ToJSON(..), FromJSON(..))
import qualified Data.ByteString.Lazy as BSL (writeFile, readFile)

import qualified UUID as UUID
import TimeUtil
import Base
import Frames (FrameRecord)
import Util (getDataPath)

data State = NotTracking | Tracking {
      project :: ProjectName
    , start :: StartTime
    , tags :: Maybe [TagName]
    } deriving (Generic, Show)
        
initState :: State
initState = NotTracking

instance FromJSON State 
instance ToJSON State 

getStateFile homeDir = joinPath [getDataPath homeDir, "state"]

stateToFrame :: ZonedTime -> Maybe UUID.UUID' -> State -> FrameRecord
stateToFrame curTime newId Tracking{tags=tags, start=start, project=project} =
    (start, stopTime, project, fromMaybe UUID.nil newId, justTags, stopTime)
    where
        stopTime = zonedTimeToUnix curTime
        justTags = fromMaybe [] (tags)

loadState :: IO (State)
loadState = do
    stateFileName <- getStateFile <$> getHomeDirectory
    fileExists <- doesFileExist stateFileName
    if not fileExists then do
        return NotTracking
    else do
        stateRaw <- BSL.readFile stateFileName
        let state = decode stateRaw :: Maybe State
        return $ fromMaybe NotTracking state

saveState :: State -> IO ()
saveState state = do
    homeDir <- getHomeDirectory
    BSL.writeFile (getStateFile homeDir) (encode state)

clearState :: IO ()
clearState = do
    homeDir <- getHomeDirectory
    removeFile (getStateFile homeDir) `catch` handleExists
      where handleExists e
              | isDoesNotExistError e = return ()
              | otherwise = throwIO e

