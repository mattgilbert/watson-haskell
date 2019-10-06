{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module TrackerData where

import System.IO

import qualified UUID as UUID
import Data.Maybe
import Data.Time.LocalTime
import Prelude hiding (catch)
import System.Directory
import Control.Exception
import System.IO.Error hiding (catch)
import GHC.Generics
import System.Directory
import System.Info
import System.FilePath
import System.Posix
import Data.Aeson (withText, Value(..), eitherDecode', encode, decode, ToJSON(..), FromJSON(..))
import qualified Data.ByteString.Lazy.Char8 as BC
import qualified Data.ByteString.Lazy.UTF8 as BSL8
import qualified Data.ByteString.Lazy as BS (writeFile, readFile)

import TimeTracker
import TimeUtil

dataPath :: FilePath -> String
dataPath userHomeDir = 
    case os of
        "darwin" -> joinPath [userHomeDir, "Library/Application Support/watson"]
        _ -> error $ "Sorry OS not supported: " ++ os

getFramesFile homeDir = joinPath [dataPath homeDir, "frames"]
getStateFile homeDir = joinPath [dataPath homeDir, "state"]

instance FromJSON State 
instance ToJSON State 

instance FromJSON UUID.UUID where
    parseJSON = withText "UUID" $
        maybe (fail "invalid UUID") pure . UUID.fromText

instance ToJSON UUID.UUID where
    toJSON = toJSON . UUID.toText
    toEncoding = undefined


loadFrames :: IO (Frames)
loadFrames = do
    homeDir <- getHomeDirectory
    let framesFileName = getFramesFile homeDir
    fileExists <- doesFileExist framesFileName
    if not fileExists then do
        return []
    else do
        framesRaw <- BS.readFile framesFileName
        -- TODO: create frame file if it doesn't exist
        let lines = BC.split ('\n') framesRaw
        let withoutNewlines = BC.concat lines
        let frames = eitherDecode' withoutNewlines
        let result = case frames of
                    Left s -> error $ "Error:"++s
                    Right f -> f
        return result

addFrame :: Frames -> FrameRecord -> IO()
addFrame frames newFrame = do
    let newFrames = frames ++ [newFrame]
    framesFileName <- getFramesFile <$> getHomeDirectory
    BS.writeFile framesFileName (encode newFrames)
    
stateToFrame :: ZonedTime -> Maybe UUID.UUID -> State -> FrameRecord
stateToFrame curTime newId state =
    (start state, round stopTime, project state, fromMaybe UUID.nil newId, [], round stopTime)
    where
        stopTime = zonedTimeToPOSIX curTime

loadState :: IO (State)
loadState = do
    stateFileName <- getStateFile <$> getHomeDirectory
    fileExists <- doesFileExist stateFileName
    if not fileExists then do
        return NotTracking
    else do
        stateRaw <- BS.readFile stateFileName
        let state = decode stateRaw :: Maybe State
        return $ fromMaybe NotTracking state

saveState :: State -> IO ()
saveState state = do
    homeDir <- getHomeDirectory
    BS.writeFile (getStateFile homeDir) (encode state)

clearState :: IO ()
clearState = do
    homeDir <- getHomeDirectory
    removeFile (getStateFile homeDir) `catch` handleExists
      where handleExists e
              | isDoesNotExistError e = return ()
              | otherwise = throwIO e

