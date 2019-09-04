{-# LANGUAGE OverloadedStrings #-}
module Main where

import System.IO
import System.Exit
import Data.List
import Data.Time.Clock
import Data.Time.LocalTime
import Data.Time.Clock.POSIX
import Data.UUID.V1
import Data.Maybe
import Control.Monad

import qualified ArgParser as Args (CommandLineArgs(..), Command(..), getArgs)
import TimeUtil
import TimeTracker
import TrackerData
import qualified Report as Report (generate)

{--
TODO:
- maybe we don't want to use a Reader? still end up with a bunch of "do" functions
? when displaying times, show "X <units> ago", e.g. "4 months ago" or "5 minutes ago"
- support for tags
- REPORT command
  - default: last 7 days, all projects, text output
  - date range
  - output type: text, json, csv
  - limit to project name
  - limit to specific tags

FUTURE:
- config: choose backend, choose file locations, etc
- abstract so we can have file or sqlite backend
--}
data CommandResult = Success [String] | Failure [String]

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

    -- using seq to force evaluation, blech
    result <- state `seq` frames `seq` runCommand cmdState

    case result of
        Success msgs -> do
            forM_ msgs putStrLn
            exitSuccess
        Failure msgs -> do
            forM_ msgs putStrLn
            exitFailure

runCommand :: CommandState -> IO (CommandResult)

-- Status
runCommand CommandState{cmd=Args.Status, state=NotTracking} =
    pure $ Success ["not currently tracking a project"]
runCommand CommandState{cmd=Args.Status, state=(Tracking proj startTime _), curTime=curTime} = do
    let tz = zonedTimeZone curTime
    let localStartTime = posixTimeToZoned tz (realToFrac startTime)
    pure $ Success [("tracking "++ proj++", started "++(show localStartTime))]

-- Start
runCommand CommandState{cmd=(Args.Start p at), state=(Tracking proj _ _)} = 
    pure $ Failure [("project " ++ proj ++ " already started!")]
runCommand CommandState{cmd=(Args.Start p at), state=NotTracking, curTime=curTime} =
    startTracking saveState p curTime at

-- Stop
runCommand CommandState{cmd=(Args.Stop at), state=NotTracking} =
    pure $ Failure ["no project started!"]
runCommand CommandState{cmd=(Args.Stop at), state=state, frames=frames, curTime=curTime} = 
    stopTracking curTime at state clearState (addFrame frames)

-- Cancel
runCommand CommandState{cmd=Args.Cancel, state=NotTracking} =
    pure $ Failure ["no project started!"]
runCommand CommandState{cmd=Args.Cancel, state=(Tracking proj _ _)} = do
    clearState
    pure $ Success [("cancelled tracking project" ++ proj)]

---- Projects
runCommand CommandState{cmd=Args.Projects, frames=frames} = do
    --if length frames == 0 then
    --    pure $ Success ["no projects yet!"]
    --else do
        let projNames = nub $ fmap (\(_, _, proj, _, _, _) -> proj) frames
        pure $ Success projNames

---- Report
runCommand CommandState{cmd=Args.Report, frames=frames, curTime=curTime} = do
    pure $ Success $ Report.generate curTime frames

startTracking :: (State -> IO()) -> ProjectName -> ZonedTime -> Maybe String -> IO (CommandResult)
startTracking addState projName curTime startTimeStr = do
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
    return $ Success [("added " ++ projName)]
    

stopTracking :: ZonedTime -> Maybe String -> State -> IO() -> (FrameRecord -> IO()) -> IO (CommandResult)
stopTracking curTime stopTimeStr state clearState addFrame = do
    let stopTime = zonedTimeToPOSIX $ fromMaybe curTime $ fmap (getTimeWithin24Hrs' curTime) stopTimeStr

    maybeNewId <- nextUUID
    let newId = fromJust maybeNewId

    addFrame (start state, round stopTime, project state, newId, [], round stopTime)
    clearState

    let friendlyTime = utcToZonedTime (zonedTimeZone curTime) $ posixSecondsToUTCTime stopTime
    let resultMsg = ("stopped tracking " ++ project state ++ " at " ++ (show friendlyTime))
    pure $ Success [resultMsg]

