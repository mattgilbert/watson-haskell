{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE DuplicateRecordFields #-}
module Main where

import System.IO
import System.Exit
import Control.Monad
import Data.Maybe
import Data.List
import Data.Time.Clock
import Data.Time.LocalTime
import Data.Time.Clock.POSIX
import Data.Time.Format
import qualified Data.Time.Calendar as Cal

import UUID
import TimeUtil
import TimeTracker
import TrackerData
import qualified ArgParser as Args
import qualified Report as Report 

data CommandResult = Success String | Failure String

data CommandState = CommandState
    { cmd :: Args.Command
    , state :: State
    , frames :: Frames
    , curTime :: ZonedTime
    }

main = do
    state <- loadState
    frames <- loadFrames
    cmd <- Args.getArgs
    curTime <- getZonedTime

    let cmdState = CommandState cmd state frames curTime

    -- using seq to force evaluation to fix lazy IO problems, blech
    result <- state `seq` frames `seq` runCommand cmdState

    case result of
        Success msg -> do
            putStrLn msg
            exitSuccess
        Failure msg -> do
            putStrLn msg
            exitFailure


runCommand :: CommandState -> IO (CommandResult)

-- Status
runCommand CommandState{cmd=Args.Status, state=NotTracking} =
    pure $ Success "not currently tracking a project"
runCommand CommandState{cmd=Args.Status, state=(Tracking proj startTime maybeTags), curTime=curTime} = do
    let tz = zonedTimeZone curTime
    let localStartTime = posixTimeToZoned tz (realToFrac startTime)
    let diff = (round $ realToFrac $ zonedTimeToPOSIX curTime) - (startTime)
    let statusText = intercalate " " $ filter (not . null) 
            [ "project"
            , proj
            , fromMaybe "" $ showTags <$> maybeTags
            , "started"
            , (show $ humanDuration diff)
            , "ago"
            ] 
    pure $ Success statusText

-- Start
runCommand CommandState{cmd=(Args.Start at p tags), state=(Tracking proj _ _)} = 
    pure $ Failure ("project " ++ proj ++ " already started!")
runCommand CommandState{cmd=(Args.Start at p tags), state=NotTracking, curTime=curTime} =
    startTracking saveState p tags curTime at

-- Restart
runCommand CommandState{cmd=(Args.Restart), state=(Tracking proj _ _)} = 
    pure $ Failure ("project " ++ proj ++ " already started!")
runCommand CommandState{cmd=(Args.Restart), frames=frames, state=NotTracking, curTime=curTime} =
    restartTracking saveState frames curTime 

-- Stop
runCommand CommandState{cmd=(Args.Stop at), state=NotTracking} =
    pure $ Failure "no project started!"
runCommand CommandState{cmd=(Args.Stop at), state=state, frames=frames, curTime=curTime} = 
    stopTracking curTime at state clearState (addFrame frames)

-- Cancel
runCommand CommandState{cmd=Args.Cancel, state=NotTracking} =
    pure $ Failure "no project started!"
runCommand CommandState{cmd=Args.Cancel, state=(Tracking proj _ _)} = do
    clearState
    pure $ Success ("cancelled tracking project" ++ proj)

---- Projects
runCommand CommandState{cmd=Args.Projects, frames=frames} = do
    if length frames == 0 then
        pure $ Success "no projects yet!"
    else do
        let projNames = intercalate "\n" $
                sort $
                unique $
                fmap (\(_, _, proj, _, _, _) -> proj) frames
        pure $ Success projNames

---- Tags
runCommand CommandState{cmd=Args.Tags, frames=frames} = do
    if length frames == 0 then
        pure $ Success "no projects yet!"
    else do
        let tagNames = intercalate "\n" $
                sort $
                unique $
                fmap (\(_, _, _, _, tags, _) -> tags) frames >>= id -- yikes
        if length tagNames == 0 then
            pure $ Success "no tags yet!"
        else
            pure $ Success tagNames

---- Frames
runCommand CommandState{cmd=Args.Frames, frames=frames} = do
    pure $ Success $ intercalate "\n" $ toString . frameId <$> frames

---- Report
runCommand CommandState{cmd=(Args.Report range useCurrent), frames=frames, curTime=curTime, state=state} = do
    let (from, to) = getRange range
    let criteria = Report.ReportCriteria from to (fromMaybe False useCurrent)
    let result = Report.generate criteria curTime state frames

    pure $ Success (Report.format result)

    where
        midnightTomorrow = midnightOf (addDays 1 curTime)
        getRange (Just (Args.Specific from to)) = (parseToZonedTime from, parseToZonedTime to)
        getRange (Just (Args.LastYear)) = (startOfYear curTime, midnightTomorrow)
        getRange (Just (Args.LastMonth)) = (startOfMonth curTime, midnightTomorrow)
        getRange (_) = (addDays (-7) midnightTomorrow, midnightTomorrow)
        -- LastWeek is implied with the default

startTracking :: (State -> IO()) -> ProjectName -> [String] -> ZonedTime -> Maybe String -> IO (CommandResult)
startTracking addState projName tags curTime startTimeStr = do
    let unixTime = zonedTimeToPOSIX curTime

    let startTime = fmap (getTimeWithin24Hrs' curTime) startTimeStr
    let startTimePOSIX = (fmap (zonedTimeToPOSIX) startTime) 

    let maybeTags = if length tags == 0 then Nothing 
                    else Just tags

    addState (
        Tracking 
            { project = projName
            , start = round $ fromMaybe unixTime startTimePOSIX
            , tags = maybeTags }
        )
    let msg = intercalate " " $ filter (not . null) $ 
            [ "started project"
            , projName
            , fromMaybe "" $ showTags <$> maybeTags
            , "at"
            , show curTime
            ]
    pure $ Success msg

restartTracking :: (State -> IO()) -> Frames -> ZonedTime -> IO (CommandResult)
restartTracking addState frames curTime = do
    if length frames == 0 then
        pure $ Failure "can't restart, no projects started yet"
    else
        startTracking addState projName tags curTime Nothing

    where 
        (_, _, projName, _, tags, _) = last frames

stopTracking :: ZonedTime -> Maybe String -> State -> IO() -> (FrameRecord -> IO()) -> IO (CommandResult)
stopTracking curTime stopTimeStr state clearState addFrame = do
    let stopTime = zonedTimeToPOSIX $ fromMaybe curTime $ fmap (getTimeWithin24Hrs' curTime) stopTimeStr

    maybeNewId <- UUID.nextUUID
    let newId = fromJust maybeNewId

    let newFrame = stateToFrame curTime (Just newId) state
    addFrame newFrame
    clearState

    let friendlyTime = utcToZonedTime (zonedTimeZone curTime) $ posixSecondsToUTCTime stopTime
    let resultMsg = ("stopped tracking " ++ project state ++ " at " ++ (show friendlyTime))
    pure $ Success resultMsg

showTags :: [String] -> String
showTags tags =
    "[" ++(intercalate ", " tags)++ "]"

unique = nub
