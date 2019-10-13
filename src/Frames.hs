{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Frames where

import Control.Exception
import Prelude hiding (catch, id)

import Data.Maybe
import Data.List
import Data.Time.LocalTime
import Data.Time.Clock.POSIX
import System.Directory
import System.Info
import System.FilePath

import GHC.Generics
import Data.Aeson.Encode.Pretty
import Data.Aeson (withText, Value(..), eitherDecode', encode, decode, decode', ToJSON(..), FromJSON(..))
import qualified Data.Aeson.Encoding as E (unsafeToEncoding)

import qualified Data.ByteString.Builder as B
import qualified Data.ByteString as BS (ByteString, filter)
import qualified Data.ByteString.Char8 as BS8 (filter)
import qualified Data.ByteString.Lazy.Char8 as BC (split, concat)
import qualified Data.ByteString.Lazy.UTF8 as BSL8
import qualified Data.ByteString.Lazy as BSL (writeFile, readFile, toStrict)

import qualified UUID as UUID
import TimeUtil
import UUID
import Util (getDataPath)
import Base

type FrameRecord = 
    ( StartTime
    , StopTime
    , ProjectName
    , Id
    , [TagName]
    , LastUpdateTime
    )

type Frames = [FrameRecord]

initFrames :: [FrameRecord]
initFrames = []

frameId :: FrameRecord -> UUID'
frameId (_, _, _, id, _, _) =
    id

frameProject :: FrameRecord -> String
frameProject (_, _, projName, _, _, _) =
    projName

frameTags :: FrameRecord -> [TagName]
frameTags (_, _, _, _, tags, _) =
    tags

frameStartTime :: FrameRecord -> POSIXTime
frameStartTime (startTime, _, _, _, _, _) =
    realToFrac startTime

frameStopTime :: FrameRecord -> Int
frameStopTime (_, stopTime, _, _, _, _) =
    stopTime

frameDuration :: FrameRecord -> Int
frameDuration (startTime, stopTime, _, _, _, _) =
    stopTime - startTime
    --diffUTCTime (toUtc stopTime) (toUtc startTime)
    --where 
    --    toUtc i = posixSecondsToUTCTime $ realToFrac i

getFramesFile homeDir = joinPath [getDataPath homeDir, "frames"]

loadFrames :: IO (Frames)
loadFrames = do
    homeDir <- getHomeDirectory
    let framesFileName = getFramesFile homeDir
    fileExists <- doesFileExist framesFileName
    if not fileExists then do
        return []
    else do
        framesRaw <- BSL.readFile framesFileName
        -- TODO: create frame file if it doesn't exist
        let lines = BC.split ('\n') framesRaw
        let withoutNewlines = BC.concat lines
        let frames = eitherDecode' withoutNewlines
        let result = case frames of
                    Left s -> error $ "Error:"++s
                    Right f -> f
        return result

findFrame :: String -> Frames -> Maybe FrameRecord
findFrame idOrIndex frames =
    find (\f -> UUID.partialStringMatch idOrIndex (frameId f)) frames

    where
        idlen = length idOrIndex

data EditableFrame = EditableFrame 
    { projectName :: ProjectName
    , startTime :: LocalTime
    , stopTime :: LocalTime
    , tags :: [TagName]
    } deriving (Generic, Show)

instance FromJSON EditableFrame 
instance ToJSON EditableFrame 

frameToJson :: ZonedTime -> FrameRecord -> BS.ByteString
frameToJson curTime (start, stop, projName, id, tags, lastupdate) = 
    BSL.toStrict $ encodePretty frameJson
    where
      localStart = zonedTimeToLocalTime $ unixToZonedTime curTime start
      localStop = zonedTimeToLocalTime $ unixToZonedTime curTime stop
      frameJson = EditableFrame projName localStart localStop tags

jsonToEditableFrame :: BSL8.ByteString -> Maybe EditableFrame
jsonToEditableFrame json = do
    decode' json

jsonToFrame :: BSL8.ByteString -> FrameRecord -> ZonedTime -> Maybe FrameRecord
jsonToFrame json origFrame curTime =
    createFrame <$> editedFrame

    where
        editedFrame = decode' json
        createFrame edited = 
            (unixStart, unixStop, projectName, frameId origFrame, tags, zonedTimeToUnix curTime)
            where
                EditableFrame {projectName=projectName, startTime=localStart, stopTime=localStop, tags=tags} = edited
                unixStart = zonedTimeToUnix $ ZonedTime localStart (zonedTimeZone curTime)
                unixStop = zonedTimeToUnix $ ZonedTime localStop (zonedTimeZone curTime)

addFrame :: Frames -> FrameRecord -> IO()
addFrame frames newFrame = do
    let newFrames = frames ++ [newFrame]
    saveFrames newFrames

updateFrame :: FrameRecord -> BSL8.ByteString -> ZonedTime -> Frames -> IO ()
updateFrame oldFrame newFrameJson curTime frames = do
    let (firstHalf, secondHalf) = 
                break (\f -> (frameId oldFrame) == (frameId f)) frames

    if (length secondHalf == 0) || (newFrame == Nothing) then
        -- TODO: should send back an error of some sort
        pure ()
    else do
        let _:newSecondHalf = secondHalf
        let newFrames = firstHalf ++ [fromJust newFrame] ++ newSecondHalf
        saveFrames newFrames
    where
        newFrame = jsonToFrame newFrameJson oldFrame curTime
    

removeFrame :: (FrameRecord -> IO(Bool)) -> Frames -> String -> IO (Maybe Bool)
removeFrame verify frames idToRemove = do
    let idlen = length idToRemove
    let (firstHalf, secondHalf) = 
                break (\f -> UUID.partialStringMatch idToRemove (frameId f)) frames

    if length secondHalf == 0 then
        pure $ Just False
    else do
        let frameToRemove:remainder = secondHalf
        shouldRemove <- verify frameToRemove
        if shouldRemove then do
            let newFrames = firstHalf ++ remainder
            saveFrames newFrames
            pure $ Just True
        else
            pure $ Nothing

saveFrames :: Frames -> IO ()
saveFrames newFrames = do
    framesFileName <- getFramesFile <$> getHomeDirectory
    BSL.writeFile framesFileName (encode newFrames)
