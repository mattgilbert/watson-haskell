module Report where

import Data.List
import Data.Time.LocalTime
import TimeTracker
import TimeUtil

data ReportCriteria = ReportCriteria {
    dateRange :: Maybe (ZonedTime, ZonedTime)
}

defaultCriteria :: ReportCriteria
defaultCriteria =
    ReportCriteria Nothing

frameWithinRange :: ZonedTime -> Maybe (ZonedTime, ZonedTime) -> FrameRecord -> Bool
frameWithinRange curTime Nothing frame =
    frameWithinRange curTime (Just (sevenDaysAgo, curTime)) frame
    where
        sevenDaysAgo = addDays (negate 7) curTime
frameWithinRange _ (Just (from, to)) frame =
    (frameStopTime frame) >= (zonedTimeToPOSIX from) &&
    (frameStopTime frame) <= (zonedTimeToPOSIX to)

-- Default:
-- everything within last 7 days, grouped by project
--    projA 10h 5m 10s
--    projB 1h 3m
generate :: ReportCriteria -> ZonedTime -> Frames -> [String]
generate ReportCriteria{dateRange=dateRange} curTime frames =
    fmap totalTimes $ 
        groupBy sameProject $ 
        filter (frameWithinRange curTime dateRange) frames
    where
        sevenDaysAgo = 
            zonedTimeToPOSIX $ addDays (negate 7) curTime
        withinLast7Days frame =
            (frameStopTime frame) > sevenDaysAgo
        sameProject p1 p2 = 
            frameProject p1 == frameProject p2
        totalTimes frames =
            (frameProject $ head frames) ++ ":" ++ 
            (humanDuration $ sum $ fmap frameDuration frames)
