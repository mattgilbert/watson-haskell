module Report where

import Data.List
import Data.Time.LocalTime
import TimeTracker
import TimeUtil

-- Default:
-- everything within last 7 days, grouped by project
--    projA 10h 5m 10s
--    projB 1h 3m
generate :: ZonedTime -> Frames -> [String]
generate curTime frames =
    fmap totalTimes $ 
        groupBy sameProject $ 
        filter withinLast7Days frames
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
