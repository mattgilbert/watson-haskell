{-# LANGUAGE OverloadedStrings #-}
module TimeUtil where

import Control.Monad
import Data.Maybe
import qualified Data.Time.Calendar as Cal
import qualified Data.Time.Format as DTF
import Data.Time
import Data.Time.Clock
import Data.Time.Clock.POSIX
import Data.Time.LocalTime

timeFormat = "%H:%M"

data DateRange = DateRange ZonedTime ZonedTime

formatTime :: ZonedTime -> Int -> String
formatTime curTime t =
    DTF.formatTime defaultTimeLocale timeFormat zonedTime
    where
        zonedTime = posixTimeToZoned (zonedTimeZone curTime) (realToFrac t)

parseRangeToZonedTime :: Maybe String -> Maybe String -> DateRange
parseRangeToZonedTime from to =
    DateRange (parseToZonedTime $ fromJust from) (parseToZonedTime $ fromJust to)

parseToZonedTime :: String -> ZonedTime
parseToZonedTime timeStr =
    parseTimeOrError True defaultTimeLocale "%0Y-%m-%d" timeStr :: ZonedTime

zonedTimeToUnix :: ZonedTime -> Int
zonedTimeToUnix zt =
    round $ utcTimeToPOSIXSeconds $ zonedTimeToUTC zt

unixToZonedTime :: ZonedTime -> Int -> ZonedTime
unixToZonedTime curTime intTime =
    utcToZonedTime (zonedTimeZone curTime) $ posixSecondsToUTCTime $ realToFrac intTime

posixTimeToZoned :: TimeZone -> POSIXTime -> ZonedTime
posixTimeToZoned tz pt =
    utcToZonedTime tz $ posixSecondsToUTCTime pt

addDays :: Integer -> ZonedTime -> ZonedTime
addDays days ZonedTime{zonedTimeToLocalTime=lt, zonedTimeZone=tz} =
    ZonedTime adjusted tz
    where
        adjusted = LocalTime (Cal.addDays days (localDay lt)) (localTimeOfDay lt)

startOfYear :: ZonedTime -> ZonedTime
startOfYear t =
    ZonedTime (LocalTime startDay midnight) tz
    where
        ZonedTime{zonedTimeToLocalTime=curLocalTime, zonedTimeZone=tz} = t
        (year, _, _) = Cal.toGregorian (localDay curLocalTime)
        startDay = Cal.fromGregorian year 1 1

startOfMonth :: ZonedTime -> ZonedTime
startOfMonth t =
    ZonedTime (LocalTime monthStartDay midnight) tz
    where
        ZonedTime{zonedTimeToLocalTime=curLocalTime, zonedTimeZone=tz} = t
        (year, month, _) = Cal.toGregorian (localDay curLocalTime)
        monthStartDay = Cal.fromGregorian year month 1

truncateSeconds :: ZonedTime -> ZonedTime
truncateSeconds t =
    adjustedZonedTime

    where
        ZonedTime{zonedTimeToLocalTime=curLocalTime, zonedTimeZone=curTimeZone} = t
        LocalTime{localTimeOfDay=curTimeOfDay, localDay=curLocalDay} = curLocalTime
        TimeOfDay{todHour=curHour, todMin=curMin} = curTimeOfDay

        adjustedTime = TimeOfDay curHour curMin 0
        adjustedLocalTime = LocalTime curLocalDay adjustedTime
        adjustedZonedTime = ZonedTime adjustedLocalTime curTimeZone
    

midnightOf :: ZonedTime -> ZonedTime
midnightOf t =
    ZonedTime (LocalTime (localDay curLocalTime) midnight) curTimeZone
    where
        ZonedTime{zonedTimeToLocalTime=curLocalTime, zonedTimeZone=curTimeZone} = t


getTimeWithin24Hrs' :: ZonedTime -> String -> ZonedTime
getTimeWithin24Hrs' curTime timeStr =
    if (zonedTimeToUTC possibleTime) > (zonedTimeToUTC curTime) then
        ZonedTime (LocalTime (Cal.addDays (negate 1) curDay) userSeconds) (zonedTimeZone curTime)
    else
        possibleTime
    where
        userSeconds = (parseTimeOrError True defaultTimeLocale timeFormat timeStr) :: TimeOfDay
        curDay = localDay $ zonedTimeToLocalTime curTime
        possibleTime = ZonedTime (LocalTime curDay userSeconds) (zonedTimeZone curTime)


-- TODO: better way to do this? can we define functions for each and compose into
-- a single conversion function?
humanDuration :: Int -> String
humanDuration totalSeconds =
    unwords 
        $ map (\(a,b) -> (show a)++b)
        $ filter (\(a,b) -> a > 0) [
            (days, "d"),
            (hours, "h"),
            (minutes, "m"),
            (seconds, "s")
        ]
    where
        days = totalSeconds `div` 86400
        hours = (totalSeconds `mod` 86400) `div` 3600
        minutes = (totalSeconds `mod` 86400 `mod` 3600) `div` 60
        seconds = (totalSeconds `mod` 86400 `mod` 3600 `mod` 60)
