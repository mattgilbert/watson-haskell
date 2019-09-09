{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE DuplicateRecordFields #-}
module Report where

import Text.Printf
import Data.List
import Data.Time.LocalTime
import Data.Time.Format
import TimeTracker
import TimeUtil

data ReportCriteria = ReportCriteria {
    dateRange :: Maybe DateRange
}

data ReportResult = ReportResult {
    dateRange :: DateRange,
    summaries :: [(String, Int)]
}

defaultCriteria :: ReportCriteria
defaultCriteria =
    ReportCriteria Nothing

format :: ReportResult -> [String]
format ReportResult{dateRange=DateRange dateStart dateEnd, summaries=summaries} =
    header:"":projects

    where 
        header = printf "Report period: %s to %s" (fmtDate dateStart) (fmtDate dateEnd)
        projects = fmap fmtProject summaries
        fmtProject (name, seconds) = intercalate "\t" [name, (humanDuration seconds)]
        fmtDate = formatTime defaultTimeLocale (dateFmt defaultTimeLocale)
    

generate :: ReportCriteria -> ZonedTime -> Frames -> ReportResult
generate ReportCriteria{dateRange=dateRange} curTime frames =
    ReportResult range times

    where
        times = fmap asSummaries $ 
            groupBy sameProject $ 
            filter (frameWithinRange range) frames

        range = determineDateRange dateRange

        determineDateRange Nothing = DateRange (addDays (negate 7) curTime) curTime
        determineDateRange (Just (dr)) = dr

        frameWithinRange (DateRange from to) frame =
            (frameStopTime frame) >= (zonedTimeToPOSIX from) &&
            (frameStopTime frame) <= (zonedTimeToPOSIX to)

        sameProject p1 p2 = 
            frameProject p1 == frameProject p2
        asSummaries frames =
            ((frameProject $ head frames), sum $ fmap frameDuration frames)
