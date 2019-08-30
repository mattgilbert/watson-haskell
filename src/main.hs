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
- add --at option for start command as well
- switch to Reader for env, include state, frames, curTime
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
-- Start
runCommand (Args.Start p) (Tracking proj _ _) _ = 
    pure $ CommandResult False ("project " ++ proj ++ " already started!")
runCommand (Args.Start p) NotTracking frames =
    startTracking saveState p

-- Stop
runCommand (Args.Stop at) NotTracking frames = 
    pure $ CommandResult False "no project started!"
runCommand (Args.Stop at) state frames = 
    stopTracking at state clearState (addFrame frames)


startTracking :: (State -> IO()) -> ProjectName -> IO (CommandResult)
startTracking addState projName = do
    unixTime <- getPOSIXTime
    addState (
        Tracking 
            { project = projName
            , start = round unixTime
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

