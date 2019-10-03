{-# LANGUAGE DeriveGeneric #-}

module TrackerData where

import System.IO

import Data.Maybe
import Data.Time.LocalTime
import qualified Data.UUID as UUID
import Prelude hiding (catch)
import System.Directory
import Control.Exception
import System.IO.Error hiding (catch)
import GHC.Generics
import System.Directory
import Data.Aeson (encode, decode, ToJSON, FromJSON)
import qualified Data.ByteString.Lazy.UTF8 as BSL8
import qualified Data.ByteString.Lazy as BS (writeFile, readFile)

import TimeTracker
import TimeUtil

framesFileName = "./frames.json"
stateFileName = "./state.json"

instance FromJSON State 
instance ToJSON State 

loadFrames :: IO (Frames)
loadFrames = do
    fileExists <- doesFileExist framesFileName
    if not fileExists then do
        return []
    else do
        framesRaw <- BS.readFile framesFileName
        -- TODO: create frame file if it doesn't exist
        let frames = fromMaybe [] $ decode framesRaw
        return frames

addFrame :: Frames -> FrameRecord -> IO()
addFrame frames newFrame = do
    let newFrames = frames ++ [newFrame]
    BS.writeFile framesFileName (encode newFrames)
    
stateToFrame :: ZonedTime -> Maybe UUID.UUID -> State -> FrameRecord
stateToFrame curTime newId state =
    (start state, round stopTime, project state, fromMaybe UUID.nil newId, [], round stopTime)
    where
        stopTime = zonedTimeToPOSIX curTime

loadState :: IO (State)
loadState = do
    fileExists <- doesFileExist stateFileName
    if not fileExists then do
        return NotTracking
    else do
        stateRaw <- BS.readFile "./state.json"
        let state = decode stateRaw :: Maybe State
        return $ fromMaybe NotTracking state

saveState :: State -> IO ()
saveState state = do
    BS.writeFile stateFileName (encode state)

clearState :: IO ()
clearState = 
    removeFile stateFileName `catch` handleExists
      where handleExists e
              | isDoesNotExistError e = return ()
              | otherwise = throwIO e

