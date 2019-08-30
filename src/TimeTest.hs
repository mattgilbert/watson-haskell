{-# LANGUAGE OverloadedStrings #-}
module TimeTest where

import Control.Monad
import Data.Maybe
import Data.Time
import Data.Time.Clock
import Data.Time.LocalTime

timeFormat = "%H:%M"

getTimeWithin24Hrs :: String -> IO (LocalTime)
getTimeWithin24Hrs timeStr = do
    curTime <- zonedTimeToLocalTime <$> getZonedTime

    let userSeconds = (parseTimeOrError True defaultTimeLocale timeFormat timeStr) :: TimeOfDay
    let curDay = localDay curTime
    let possibleTime = LocalTime curDay userSeconds

    let adjusted = if possibleTime < curTime then
                       LocalTime (addDays (negate 1) curDay) userSeconds
                   else
                       possibleTime

    return adjusted


getTimeWithin24Hrs' :: ZonedTime -> String -> ZonedTime
getTimeWithin24Hrs' curTime timeStr = do
    --curTime <- zonedTimeToLocalTime <$> getZonedTime

    let userSeconds = (parseTimeOrError True defaultTimeLocale timeFormat timeStr) :: TimeOfDay
    let curDay = localDay $ zonedTimeToLocalTime curTime
    let possibleTime = ZonedTime (LocalTime curDay userSeconds) (zonedTimeZone curTime)

    if (zonedTimeToUTC possibleTime) > (zonedTimeToUTC curTime) then
        ZonedTime (LocalTime (addDays (negate 1) curDay) userSeconds) (zonedTimeZone curTime)
    else
        possibleTime


getTimeWithin24Hrs'' :: String -> IO (Maybe LocalTime)
getTimeWithin24Hrs'' timeStr = do
    curTime <- zonedTimeToLocalTime <$> getZonedTime

    --let userSeconds = (parseTimeOrError True defaultTimeLocale timeFormat timeStr) :: TimeOfDay
    let userSeconds = (parseTimeM True defaultTimeLocale timeFormat timeStr) :: Maybe TimeOfDay
    let curDay = localDay curTime
    let possibleTime = LocalTime curDay <$> userSeconds

    let greaterThanNow = ((>) curTime) <$> possibleTime

    let adjusted = if fromMaybe True greaterThanNow then
                       LocalTime (addDays (negate 1) curDay) <$> userSeconds
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


