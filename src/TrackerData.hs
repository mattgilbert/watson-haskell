{-# LANGUAGE DeriveGeneric #-}

module TrackerData where

import Data.Maybe
import Prelude hiding (catch)
import System.Directory
import Control.Exception
import System.IO.Error hiding (catch)
import GHC.Generics
import System.Directory
import Data.Aeson (encode, decode, ToJSON, FromJSON)
import Data.ByteString.Lazy as BS (writeFile, readFile)

import TimeTracker

framesFileName = "./frames.json"

loadFrames :: IO (Frames)
loadFrames = do
    framesRaw <- BS.readFile framesFileName
    -- TODO: create frame file if it doesn't exist
    let frames = fromMaybe [] $ decode framesRaw 
    return frames

addFrame :: Frames -> FrameRecord -> IO()
addFrame frames newFrame = do
    let newFrames = frames ++ [newFrame]
    BS.writeFile framesFileName (encode newFrames)
    
stateFileName = "./state.json"

loadState :: IO (Maybe State)
loadState = do
    fileExists <- doesFileExist stateFileName
    if not fileExists then do
        return Nothing
    else do
        stateRaw <- BS.readFile "./state.json"
        let state = decode stateRaw :: Maybe State
        return state

saveState :: State -> IO ()
saveState state = do
    BS.writeFile stateFileName (encode state)

clearState :: IO ()
clearState = removeFile stateFileName `catch` handleExists
  where handleExists e
          | isDoesNotExistError e = return ()
          | otherwise = throwIO e

instance FromJSON State 
instance ToJSON State 
