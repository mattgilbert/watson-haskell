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

import TimeUtil
import TimeTracker
import TrackerData
import qualified ArgParser as Args (CommandLineArgs(..), Command(..), getArgs)

{--
TODO:
- maybe we don't want to use a Reader? still end up with a bunch of "do" functions
- when displaying times, show "X <units> ago", e.g. "4 months ago" or "5 minutes ago"
- support for tags
- "tags": list all tags
FUTURE:
- config: choose backend, choose file locations, etc
- abstract so we can have file or sqlite backend
--}
data CommandResult = Success [String] | Failure [String]

main = do
    state <- loadState
    frames <- loadFrames
    cmd <- Args.getArgs

    -- using seq to force evaluation, blech
    result <- state `seq` frames `seq` runCommand cmd state frames

    case result of
        Success msgs -> do
            forM_ msgs putStrLn
            exitSuccess
        Failure msgs -> do
            forM_ msgs putStrLn
            exitFailure


runCommand :: Args.Command -> State -> Frames -> IO (CommandResult)

-- Status
runCommand (Args.Status) NotTracking _ =
    pure $ Success ["not currently tracking a project"]
runCommand (Args.Status) (Tracking proj startTime _) _ = do
    tz <- getCurrentTimeZone
    let localStartTime = posixTimeToZoned tz (realToFrac startTime)
    pure $ Success [("tracking "++ proj++", started "++(show localStartTime))]

-- Start
runCommand (Args.Start p at) (Tracking proj _ _) _ = 
    pure $ Failure [("project " ++ proj ++ " already started!")]
runCommand (Args.Start p at) NotTracking frames =
    startTracking saveState p at

-- Stop
runCommand (Args.Stop at) NotTracking frames = 
    pure $ Failure ["no project started!"]
runCommand (Args.Stop at) state frames = 
    stopTracking at state clearState (addFrame frames)

-- Cancel
runCommand (Args.Cancel) NotTracking frames =
    pure $ Failure ["no project started!"]
runCommand (Args.Cancel) (Tracking proj _ _) frames = do
    clearState
    pure $ Success [("cancelled tracking project" ++ proj)]

-- Projects
runCommand (Args.Projects) _ frames = do
    --if length frames == 0 then
    --    pure $ Success ["no projects yet!"]
    --else do
        let projNames = nub $ fmap (\(_, _, proj, _, _, _) -> proj) frames
        pure $ Success projNames

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
    return $ Success [("added " ++ projName)]
    

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
    pure $ Success [resultMsg]

