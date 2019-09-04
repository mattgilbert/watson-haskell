{-# LANGUAGE OverloadedStrings #-}
module TimeUtil where

import Control.Monad
import Data.Maybe
import qualified Data.Time.Calendar as Cal
import Data.Time
import Data.Time.Clock
import Data.Time.Clock.POSIX
import Data.Time.LocalTime

timeFormat = "%H:%M"

zonedTimeToPOSIX :: ZonedTime -> POSIXTime
zonedTimeToPOSIX zt =
    utcTimeToPOSIXSeconds $ zonedTimeToUTC zt

posixTimeToZoned :: TimeZone -> POSIXTime -> ZonedTime
posixTimeToZoned tz pt =
    utcToZonedTime tz $ posixSecondsToUTCTime pt

addDays :: Integer -> ZonedTime -> ZonedTime
addDays days ZonedTime{zonedTimeToLocalTime=lt, zonedTimeZone=tz} =
    ZonedTime adjusted tz
    where
        adjusted = LocalTime (Cal.addDays days (localDay lt)) (localTimeOfDay lt)


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


-- here's a weird recursion thing that baffled me for a bit
humanDuration'' :: Int -> String
humanDuration'' seconds =
    unwords 
        $ map (\(a,b) -> (show a)++b)
        $ filter (\(a,b) -> a > 0) [
            (days, "d"),
            (hours, "h"),
            (minutes, "m"),
            (seconds, "s")
        ]
    where
        days = seconds `div` 86400
        hours = (seconds `mod` 86400) `div` 3600
        minutes = (seconds `mod` 86400 `mod` 3600) `div` 60
        seconds = (seconds `mod` 86400 `mod` 3600 `mod` 60)

getTimeWithin24Hrs :: String -> IO (LocalTime)
getTimeWithin24Hrs timeStr = do
    curTime <- zonedTimeToLocalTime <$> getZonedTime

    let userSeconds = (parseTimeOrError True defaultTimeLocale timeFormat timeStr) :: TimeOfDay
    let curDay = localDay curTime
    let possibleTime = LocalTime curDay userSeconds

    let adjusted = if possibleTime < curTime then
                       LocalTime (Cal.addDays (negate 1) curDay) userSeconds
                   else
                       possibleTime

    return adjusted


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



getTimeWithin24Hrs'' :: String -> IO (Maybe LocalTime)
getTimeWithin24Hrs'' timeStr = do
    curTime <- zonedTimeToLocalTime <$> getZonedTime

    --let userSeconds = (parseTimeOrError True defaultTimeLocale timeFormat timeStr) :: TimeOfDay
    let userSeconds = (parseTimeM True defaultTimeLocale timeFormat timeStr) :: Maybe TimeOfDay
    let curDay = localDay curTime
    let possibleTime = LocalTime curDay <$> userSeconds

    let greaterThanNow = ((>) curTime) <$> possibleTime

    let adjusted = if fromMaybe True greaterThanNow then
                       LocalTime (Cal.addDays (negate 1) curDay) <$> userSeconds
                   else
                       possibleTime

    return adjusted


{--
NOTES:
- setup
  - tab for nvim, dark mode
  - tab for terminal, light mode
  - project setup using cabal for simplicity
- phase 1
    - simple, including IO
      - return type polymorphism on parseTimeXXX
      - usage with catch for error handling
    - remove IO to keep it pure, take curTime as param
    - discuss parseTimeOrError
    - switch to parseTimeM
    - discuss use of <$>
      - good to get used to syntax: used a lot e.g. applicatie
      - carries the Nothing all the way through
    - similarly, the <$> usage avoids more if statements
      - that if statement bothers me: 1) ugh, the formatting, 2) is there a more elegant way?
--}


