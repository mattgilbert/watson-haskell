{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE DuplicateRecordFields #-}
module Report where

import Text.Printf
import Data.Maybe
import Data.List
import Data.Time.LocalTime
import Data.Time.Format

import TimeTracker
import TimeUtil
import TrackerData

data ReportCriteria = ReportCriteria {
    dateStart :: ZonedTime,
    dateEnd :: ZonedTime,
    useCurrent :: Bool
} deriving Show

data ReportResult = ReportResult {
    dateRange :: DateRange,
    summaries :: [(String, Int)]
}


format :: ReportResult -> [String]
format ReportResult{dateRange=DateRange dateStart dateEnd, summaries=summaries} =
    header:"":projects

    where 
        header = printf "Report period: %s to %s" (fmtDate dateStart) (fmtDate dateEnd)
        projects = fmap fmtProject summaries
        fmtProject (name, seconds) = intercalate "\t" [name, (humanDuration seconds)]
        fmtDate = formatTime defaultTimeLocale (dateFmt defaultTimeLocale)
    

generate :: ReportCriteria -> ZonedTime -> State -> Frames -> ReportResult
generate ReportCriteria{dateStart=dateStart, dateEnd=dateEnd, useCurrent=useCurrent} curTime state frames =
    ReportResult range times

    where
        allFrames = if useCurrent then
                        (stateToFrame curTime Nothing state):frames
                    else
                        frames
                        
        times = fmap asSummaries $ 
            groupBy sameProject $ 
            filter (frameWithinRange range) allFrames

        range = DateRange dateStart dateEnd

        ZonedTime{zonedTimeToLocalTime=curLocalTime, zonedTimeZone=curTimeZone} = curTime
        midnight = TimeOfDay 0 0 0
        midnightToday = ZonedTime (LocalTime (localDay curLocalTime) midnight) curTimeZone
        determineDateRange Nothing = DateRange (addDays (-7) midnightToday) (addDays 1 midnightToday)
        determineDateRange (Just (dr)) = dr

        frameWithinRange (DateRange from to) frame =
            (frameStopTime frame) >= (zonedTimeToPOSIX from) &&
            (frameStopTime frame) <= (zonedTimeToPOSIX to)

        sameProject p1 p2 = 
            frameProject p1 == frameProject p2
        asSummaries frames =
            ((frameProject $ head frames), sum $ fmap frameDuration frames)
