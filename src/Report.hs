module Report where

import Text.Printf
import Data.Maybe
import Data.List
import Data.Time.LocalTime

import TimeUtil
import Frames
import State

data ReportCriteria = ReportCriteria {
    dateStart :: ZonedTime,
    dateEnd :: ZonedTime,
    useCurrent :: Bool
} deriving Show

data ReportResult = ReportResult {
    dateRange :: DateRange,
    summaries :: [(String, Int)]
}


format :: ReportResult -> String
format ReportResult{dateRange=DateRange dateStart dateEnd, summaries} =
    intercalate "\n" (header:"":projects)

    where 
        header = printf "Report period: %s to %s" (formatDate dateStart) (formatDate dateEnd)
        projects = fmap formatProject summaries
        
        formatProject (name, seconds) = intercalate "\t" [name, (humanDuration seconds)]
    

generate :: ReportCriteria -> ZonedTime -> State -> Frames -> ReportResult
generate ReportCriteria{dateStart, dateEnd, useCurrent} curTime state frames =
    ReportResult range times

    where
        times = fmap asSummaries $ 
            groupBy sameProject $ 
            filter (frameWithinRange range) allFrames

        allFrames = if useCurrent then
                        (stateToFrame curTime Nothing state):frames
                    else
                        frames
                        
        range = DateRange dateStart dateEnd

        frameWithinRange (DateRange from to) frame =
            (frameStopTime frame) >= (zonedTimeToUnix from) &&
            (frameStopTime frame) <= (zonedTimeToUnix to)

        sameProject p1 p2 = 
            frameProject p1 == frameProject p2

        asSummaries frames =
            ((frameProject $ head frames), sum $ fmap frameDuration frames)
