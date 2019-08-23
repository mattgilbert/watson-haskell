{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.List
import Options.Applicative
import Data.Time.Clock.POSIX
import Data.UUID.V1
import Data.Maybe

import TimeTracker
import TrackerData
import qualified ArgParser as Args (CommandLineArgs(..), Command(..), getArgs)

main = do
    -- TODO Reader monad to store this in an environment?
    state <- loadState
    frames <- loadFrames

    cmd <- Args.getArgs
    actionResultMsg <- runCommand cmd state frames

    -- TODO: saveEnv instead of commands running against file system
    -- TODO: commands should only generate new state (maybe mutate since frames could be huge)

    print actionResultMsg


runCommand :: Args.Command -> Maybe State -> Frames -> IO(String)
runCommand (Args.Start p) state frames = startTracking state saveState  p
runCommand Args.Stop state frames = stopTracking state clearState (addFrame frames)
-- TODO: status, edit, cancel, restart, report

startTracking :: Maybe State -> (State -> IO()) -> ProjectName -> IO (String)
startTracking (Just s) _ _ = 
    pure $ "project " ++ project s ++ " already started!"
startTracking Nothing addState projName = do
    unixTime <- getPOSIXTime
    addState (
        State 
            { project = projName
            , start = round unixTime
            , tags = Nothing }
        )
    return $ "added " ++ projName
    

-- TODO: add optional stopAt param
stopTracking :: Maybe State -> (IO()) -> (FrameRecord -> IO()) -> IO (String)
stopTracking Nothing _ _ = pure "no project started!"
stopTracking (Just state) clearState addFrame = do
    unixTime <- getPOSIXTime
    let stopTime = round unixTime
    maybeNewId <- nextUUID
    let newId = fromJust maybeNewId

    addFrame (start state, stopTime, project state, newId, [], stopTime)
    clearState
    pure ("stopped tracking " ++ project state)

