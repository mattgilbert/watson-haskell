{-# LANGUAGE DeriveGeneric #-}

module TimeTracker where

import GHC.Generics
import Data.UUID

type StartTime = Int
type StopTime = Int
type ProjectName = String
type Id = UUID
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

data State = NotTracking | State {
      project :: ProjectName
    , start :: StartTime
    , tags :: Maybe [TagName]
    } deriving (Generic, Show)
        
initState :: State
initState = NotTracking

initFrames :: [FrameRecord]
initFrames = []
