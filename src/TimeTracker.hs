{-# LANGUAGE DeriveGeneric #-}

module TimeTracker where

import GHC.Generics
import UUID
import Data.Time.Clock
import Data.Time.Clock.POSIX

import TimeUtil

type StartTime = Int
type StopTime = Int
type ProjectName = String
type Id = UUID'
type TagName = String
type LastUpdateTime = Int
type Version = String

type FrameRecord = 
    ( StartTime
    , StopTime
    , ProjectName
    , Id
    , [TagName]
    , LastUpdateTime
    )
type Frames = [FrameRecord]

data State = NotTracking | Tracking {
      project :: ProjectName
    , start :: StartTime
    , tags :: Maybe [TagName]
    } deriving (Generic, Show)
        
initState :: State
initState = NotTracking

initFrames :: [FrameRecord]
initFrames = []

frameId :: FrameRecord -> UUID'
frameId (_, _, _, id, _, _) =
    id

frameProject :: FrameRecord -> String
frameProject (_, _, projName, _, _, _) =
    projName

frameStartTime :: FrameRecord -> POSIXTime
frameStartTime (startTime, _, _, _, _, _) =
    realToFrac startTime

frameStopTime :: FrameRecord -> POSIXTime
frameStopTime (_, stopTime, _, _, _, _) =
    realToFrac stopTime

frameDuration :: FrameRecord -> Int
frameDuration (startTime, stopTime, _, _, _, _) =
    stopTime - startTime
    --diffUTCTime (toUtc stopTime) (toUtc startTime)
    --where 
    --    toUtc i = posixSecondsToUTCTime $ realToFrac i
