module Main where

import System.IO
import System.Exit
import Control.Monad
import Data.Maybe
import Data.List
import Data.Time.Clock
import Data.Time.LocalTime
import Data.Time.Clock.POSIX
import qualified Data.Time.Calendar as Cal
import qualified Util
import qualified Data.ByteString.Lazy as BSL (fromStrict)

import UUID
import TimeUtil as TU
import Base
import Frames
import State
import qualified ArgParser as Args
import qualified Report as Report 

data CommandResult = Success String | Failure String

data ExecutionState = ExecutionState
    { cmd :: Args.Command
    , state :: State
    , frames :: Frames
    , curTime :: ZonedTime
    }

main = do
    tryState <- loadState
    state <- case tryState of
        Left msg -> do
            print $ "Failed to load current project: " ++ msg
            exitFailure
        Right s -> do
            pure s

    tryFrames <- loadFrames
    frames <- case tryFrames of
        Left msg -> do
            print $ "Failed to load frames: " ++ msg
            exitFailure
        Right f -> do
            pure f
    
    cmd <- Args.getArgs
    curTime <- getZonedTime

    let cmdState = ExecutionState cmd state frames curTime

    -- using seq to force evaluation to fix lazy IO problems, blech
    result <- state `seq` frames `seq` runCommand cmdState

    case result of
        Success msg -> do
            putStrLn msg
            exitSuccess
        Failure msg -> do
            putStrLn msg
            exitFailure


runCommand :: ExecutionState -> IO (CommandResult)

-- Status
runCommand ExecutionState{cmd=Args.Status, state=NotTracking} =
    pure $ Success "not currently tracking a project"
runCommand ExecutionState{cmd=Args.Status, state=(Tracking proj startTime maybeTags), curTime} = do
    let tz = zonedTimeZone curTime
    let localStartTime = posixTimeToZoned tz (realToFrac startTime)
    let diff = (zonedTimeToUnix curTime) - (startTime)
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
runCommand ExecutionState{cmd=(Args.Start at p tags), state=(Tracking proj _ _)} = 
    pure $ Failure ("project " ++ proj ++ " already started!")
runCommand ExecutionState{cmd=(Args.Start at p tags), state=NotTracking, curTime} =
    startTracking saveState p tags curTime at

-- Restart
runCommand ExecutionState{cmd=(Args.Restart), state=(Tracking proj _ _)} = 
    pure $ Failure ("project " ++ proj ++ " already started!")
runCommand ExecutionState{cmd=(Args.Restart), frames, state=NotTracking, curTime} =
    restartTracking saveState frames curTime 

-- Stop
runCommand ExecutionState{cmd=(Args.Stop at), state=NotTracking} =
    pure $ Failure "no project started!"
runCommand ExecutionState{cmd=(Args.Stop at), state, frames, curTime} = 
    stopTracking curTime at state clearState (addFrame frames)

-- Cancel
runCommand ExecutionState{cmd=Args.Cancel, state=NotTracking} =
    pure $ Failure "no project started!"
runCommand ExecutionState{cmd=Args.Cancel, state=(Tracking proj _ _)} = do
    clearState
    pure $ Success ("cancelled tracking project" ++ proj)

-- Remove
runCommand ExecutionState{cmd=Args.Remove frameId, frames, curTime} = do
    success <- removeFrame userVerify frames frameId
    case success of
        Nothing -> pure $ Success ""
        Just True -> pure $ Success "frame removed"
        Just False -> pure $ Failure "unknown frame"

    where
        userVerify :: FrameRecord -> IO(Bool)
        userVerify (start, stop, projName, _, tags, _) = do
            let msg = intercalate " " $ filter (not . null) $ 
                    [ projName
                    , if length tags == 0 then [] else showTags tags
                    , "from"
                    , TU.formatTime curTime start
                    , "to"
                    , TU.formatTime curTime stop
                    ]
            putStr $ "You are about to remove frame " ++ msg ++ ", continue? [y/N] "
            answer <- getLine
            pure $ case answer of
                "y" -> True
                "Y" -> True
                _ -> False

-- Edit
-- TODO: fix maybe/case..of nesting hell
runCommand ExecutionState{cmd=Args.Edit frameId, frames, curTime} = do
    let frame = findFrame frameId frames
    case frame of
        Nothing -> pure $ Success "unknown frame"
        Just f -> editFrame f

    where
        editFrame oldFrame = do
            let frameJson = frameToJson curTime oldFrame
            result <- Util.openEditor frameJson
            case result of
                Nothing -> 
                    pure $ Success "no changes made"
                Just result -> do
                    let newFrameJson = BSL.fromStrict result
                    updateFrame oldFrame newFrameJson curTime frames
                    pure $ Success "frame updated"

-- Projects
runCommand ExecutionState{cmd=Args.Projects, frames} = do
    if length frames == 0 then
        pure $ Success "no projects yet!"
    else do
        let projNames = intercalate "\n" $
                sort $
                unique $
                fmap frameProject frames
        pure $ Success projNames

---- Tags
runCommand ExecutionState{cmd=Args.Tags, frames} = do
    if length frames == 0 then
        pure $ Success "no projects yet!"
    else do
        let tagNames = intercalate "\n" $
                sort $
                unique $
                fmap frameTags frames >>= Prelude.id -- yikes
        if length tagNames == 0 then
            pure $ Success "no tags yet!"
        else
            pure $ Success tagNames

---- Frames
runCommand ExecutionState{cmd=Args.Frames, frames} = do
    pure $ Success $ intercalate "\n" $ toString . frameId <$> frames

---- Report
runCommand ExecutionState{cmd=(Args.Report range useCurrent), frames, curTime, state} = do
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
    let startTimeArg = (getTimeWithin24Hrs' curTime) <$> startTimeStr
    let startTime = fromMaybe (truncateSeconds curTime) startTimeArg

    let maybeTags = if length tags == 0 then Nothing 
                    else Just tags

    addState $ Tracking projName (zonedTimeToUnix startTime) maybeTags

    let msg = intercalate " " $ filter (not . null) $ 
            [ "started project"
            , projName
            , fromMaybe "" $ showTags <$> maybeTags
            , "at"
            , show startTime
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
    maybeNewId <- UUID.nextUUID
    let newId = fromJust maybeNewId

    let newFrame = stateToFrame curTime (Just newId) state
    addFrame newFrame
    clearState

    let stopTime = unixToZonedTime curTime (frameStopTime newFrame)
    let resultMsg = ("stopped tracking " ++ project state ++ " at " ++ (show stopTime))
    pure $ Success resultMsg

showTags :: [String] -> String
showTags tags =
    "[" ++(intercalate ", " tags)++ "]"

unique = nub
