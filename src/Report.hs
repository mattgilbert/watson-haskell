{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE DuplicateRecordFields #-}
module Report where

import Data.List
import Data.Time.LocalTime
import TimeTracker
import TimeUtil

data ReportCriteria = ReportCriteria {
    dateRange :: Maybe (ZonedTime, ZonedTime)
}

data ReportResult = ReportResult {
    dateRange :: (ZonedTime, ZonedTime),
    summaries :: [String]
}

defaultCriteria :: ReportCriteria
defaultCriteria =
    ReportCriteria Nothing

generate :: ReportCriteria -> ZonedTime -> Frames -> ReportResult
generate ReportCriteria{dateRange=dateRange} curTime frames =
    ReportResult range times

    where
        times = fmap totalTimes $ 
            groupBy sameProject $ 
            filter (frameWithinRange range) frames

        range = determineDateRange dateRange

        determineDateRange Nothing = (addDays (negate 7) curTime, curTime)
        determineDateRange (Just (from, to)) = (from, to)

        frameWithinRange (from, to) frame =
            (frameStopTime frame) >= (zonedTimeToPOSIX from) &&
            (frameStopTime frame) <= (zonedTimeToPOSIX to)

        sameProject p1 p2 = 
            frameProject p1 == frameProject p2
        totalTimes frames =
            (frameProject $ head frames) ++ ":" ++ 
            (humanDuration $ sum $ fmap frameDuration frames)
