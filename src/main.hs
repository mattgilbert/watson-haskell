{-# LANGUAGE OverloadedStrings #-}
module Main where

import System.IO
import System.Exit
import Data.List
import Options.Applicative
import Data.Time.Clock
import Data.Time.LocalTime
import Data.Time.Clock.POSIX
import Data.UUID.V1
import Data.Maybe

import TimeUtil
import TimeTracker
import TrackerData
import qualified ArgParser as Args (CommandLineArgs(..), Command(..), getArgs)

{--
TODO:
- maybe we don't want to use a Reader? still end up with a bunch of "do" functions
- "status" command:
   - "Project testing123 started 4 months ago (2019.04.27 21:52:11-0500)"
- "cancel" command
- support for tags
- "tags": list all tags
FUTURE:
- abstract so we can have file or sqlite backend
--}
data CommandResult = CommandResult Bool String

main = do
    state <- loadState
    frames <- loadFrames
    cmd <- Args.getArgs

    -- using seq to force evaluation, blech
    CommandResult result msg <- state `seq` frames `seq` runCommand cmd state frames

    print msg

    if result then
        exitSuccess
    else
        exitFailure


runCommand :: Args.Command -> State -> Frames -> IO(CommandResult)

-- Status
runCommand (Args.Status) NotTracking _ =
    pure $ CommandResult True "not currently tracking a project"
runCommand (Args.Status) (Tracking proj startTime _) _ = do
    tz <- getCurrentTimeZone
    let localStartTime = posixTimeToZoned tz (realToFrac startTime)
    pure $ CommandResult True ("tracking "++ proj++", started "++(show localStartTime))

-- Start
runCommand (Args.Start p at) (Tracking proj _ _) _ = 
    pure $ CommandResult False ("project " ++ proj ++ " already started!")
runCommand (Args.Start p at) NotTracking frames =
    startTracking saveState p at

-- Stop
runCommand (Args.Stop at) NotTracking frames = 
    pure $ CommandResult False "no project started!"
runCommand (Args.Stop at) state frames = 
    stopTracking at state clearState (addFrame frames)

-- Cancel
runCommand (Args.Cancel) NotTracking frames =
    pure $ CommandResult False "no project started!"
runCommand (Args.Cancel) (Tracking proj _ _) frames = do
    clearState
    pure $ CommandResult True ("cancelled tracking project" ++ proj)


startTracking :: (State -> IO()) -> ProjectName -> Maybe String -> IO (CommandResult)
startTracking addState projName startTimeStr = do
    curTime <- getZonedTime

    let unixTime = zonedTimeToPOSIX curTime

    -- TODO: can probably use bind here somehow and streamline this or turn into composition?
    let startTime = fmap (getTimeWithin24Hrs' curTime) startTimeStr
    let startTimePOSIX = (fmap (zonedTimeToPOSIX) startTime) 

    addState (
        Tracking 
            { project = projName
            , start = round $ fromMaybe unixTime startTimePOSIX
            , tags = Nothing }
        )
    return $ CommandResult True ("added " ++ projName)
    

stopTracking :: Maybe String -> State -> (IO()) -> (FrameRecord -> IO()) -> IO (CommandResult)
stopTracking stopTimeStr state clearState addFrame = do
    curTime <- getZonedTime
    stopTime <- case stopTimeStr of
                    Nothing -> do
                        getPOSIXTime
                    Just s -> do
                        let zonedStopTime = getTimeWithin24Hrs' curTime s
                        pure $ utcTimeToPOSIXSeconds $ zonedTimeToUTC zonedStopTime

    maybeNewId <- nextUUID
    let newId = fromJust maybeNewId

    addFrame (start state, round stopTime, project state, newId, [], round stopTime)
    clearState

    let friendlyTime = utcToZonedTime (zonedTimeZone curTime) $ posixSecondsToUTCTime stopTime
    let resultMsg = ("stopped tracking " ++ project state ++ " at " ++ (show friendlyTime))
    pure $ CommandResult True resultMsg

