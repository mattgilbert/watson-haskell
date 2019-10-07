{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE DuplicateRecordFields #-}
module Main where

import System.IO
import System.Exit
import Data.List
import Data.Time.Clock
import Data.Time.LocalTime
import Data.Time.Clock.POSIX
import Data.Time.Format
import qualified Data.Time.Calendar as Cal
import Data.Maybe
import Control.Monad

import qualified ArgParser as Args --(CommandLineArgs(..), Command(..), getArgs)
import UUID
import TimeUtil
import TimeTracker
import TrackerData
import qualified Report as Report 

{--
TODO:
- report
    - output type: text, json, csv
    - limit to project name
    - limit to specific tags
- refactor/improve
  - parseWatsonTags
  - move aeson instances to separate file
  - change UUID to UUID' and get rid of silly GUID prefix
  - tags: State uses Maybe [], frame uses []... get these consistent
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

    print cmd

    let cmdState = CommandState (watsonifyArgs cmd) state frames curTime

    -- using seq to force evaluation, blech
    result <- state `seq` frames `seq` runCommand cmdState

    case result of
        Success msgs -> do
            forM_ msgs putStrLn
            exitSuccess
        Failure msgs -> do
            forM_ msgs putStrLn
            exitFailure

watsonifyArgs :: Args.Command -> Args.Command
watsonifyArgs (Args.Start at projName tags) =
    -- watson cli takes tags starting with + so that tag names can have
    -- spaces, *and* so can project name. optparse doesn't really work like
    -- that, so we'll monkey with the args here for now
    Args.Start at watsonProjName watsonTags
    where
      (projNameRemainder, remainingTags) = span (\(c:_) -> c /= '+') tags
      watsonProjName = projName ++ " " ++ (intercalate " " projNameRemainder)

      watsonTags = stripPlus <$> parseWatsonTags [] remainingTags
      stripPlus ('+':t) = t
      stripPlus t = t

watsonifyArgs cmd = cmd

parseWatsonTags :: [String] -> [String] -> [String]
parseWatsonTags actualTags [] = actualTags
parseWatsonTags actualTags (tagStart:remainingWords) =
    parseWatsonTags (actualTags ++ [(intercalate " " (tagStart:nextTagWords))]) remaining
    where
        (nextTagWords, remaining) = break (\(c:_) -> c == '+') remainingWords


runCommand :: CommandState -> IO (CommandResult)

-- Status
runCommand CommandState{cmd=Args.Status, state=NotTracking} =
    pure $ Success ["not currently tracking a project"]
runCommand CommandState{cmd=Args.Status, state=(Tracking proj startTime _), curTime=curTime} = do
    let tz = zonedTimeZone curTime
    let localStartTime = posixTimeToZoned tz (realToFrac startTime)
    let diff = (round $ realToFrac $ zonedTimeToPOSIX curTime) - (startTime)
    pure $ Success [("tracking "++ proj++", started "++(show $ humanDuration diff))]

-- Start
runCommand CommandState{cmd=(Args.Start at p tags), state=(Tracking proj _ _)} = 
    pure $ Failure [("project " ++ proj ++ " already started!")]
runCommand CommandState{cmd=(Args.Start at p tags), state=NotTracking, curTime=curTime} =
    startTracking saveState p tags curTime at

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
runCommand CommandState{cmd=(Args.Report range useCurrent), frames=frames, curTime=curTime, state=state} = do
    let midnightToday = midnightOf curTime
    let (from, to) =
            case (fromJust range) of
                Args.Specific from to -> (parseToZonedTime from, parseToZonedTime to)
                Args.LastYear -> (startOfYear curTime, midnightToday)
                Args.LastMonth -> (startOfMonth curTime, midnightToday)
                _ -> (addDays (-7) midnightToday, midnightToday)
                -- LastWeek is implied with the default

    let criteria = Report.ReportCriteria from to useCurrent
    let result = Report.generate criteria curTime state frames

    pure $ Success $ Report.format result

startTracking :: (State -> IO()) -> ProjectName -> [String] -> ZonedTime -> Maybe String -> IO (CommandResult)
startTracking addState projName tags curTime startTimeStr = do
    let unixTime = zonedTimeToPOSIX curTime

    -- TODO: can probably use bind here somehow and streamline this or turn into composition?
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
    return $ Success [("added " ++ projName)]


stopTracking :: ZonedTime -> Maybe String -> State -> IO() -> (FrameRecord -> IO()) -> IO (CommandResult)
stopTracking curTime stopTimeStr state clearState addFrame = do
    let stopTime = zonedTimeToPOSIX $ fromMaybe curTime $ fmap (getTimeWithin24Hrs' curTime) stopTimeStr

    maybeNewId <- UUID.nextUUID
    let newId = fromJust maybeNewId

    let newFrame = stateToFrame curTime (Just newId) state
    --addFrame (start state, round stopTime, project state, newId, [], round stopTime)
    addFrame newFrame
    clearState

    let friendlyTime = utcToZonedTime (zonedTimeZone curTime) $ posixSecondsToUTCTime stopTime
    let resultMsg = ("stopped tracking " ++ project state ++ " at " ++ (show friendlyTime))
    pure $ Success [resultMsg]

