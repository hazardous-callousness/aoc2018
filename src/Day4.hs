{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Day4 where
import           Common
import           Data.Array                    as A (accum, elems, listArray)
import           Data.Function                 (on)
import           Data.List                     (elemIndex, maximumBy, sort)
import           Data.Map.Strict               as M (alter, assocs, empty)
import           Data.Maybe                    (fromMaybe)
import           Text.ParserCombinators.Parsec

-- [1518-11-01 00:00] Guard #10 begins shift
-- [1518-11-01 00:05] falls asleep
-- [1518-11-01 00:25] wakes up
parseInput = parse file "" . unlines . sort . lines
  where
    file = duty `endBy` many (oneOf ", \n")
    skipToMinute = count 15 anyChar
    skipToText = many digit >> string "] "
    skipToNextLine = many (noneOf "\n") >> char '\n'
    duty = do
      guardId<-parseDuty
      sleeps<-many (try asleep <|> try awake)
      return (guardId,sleeps)
    parseDuty = do
      parseTime
      string "Guard #"
      guardId<-readInt
      skipToNextLine
      pure guardId
    asleep = do
      time<-parseTime
      string "falls asleep"
      skipToNextLine
      pure time
    awake = do
      time<-parseTime
      string "wakes up"
      skipToNextLine
      pure time
    parseTime = do
      many (noneOf ":")
      char ':'
      time<-readInt
      string "] "
      pure time

exercise1 = exercise' sum

exercise2 = exercise' maximum



exercise' selector = solution . maxGuard . M.assocs . sleepingSchedule . eitherError . parseInput
  where
    maxGuard = maximumBy (compare `on` (selector . snd))
    solution (gid,minutes) = gid * minute
      where Just minute = elemIndex (maximum minutes) minutes

sleepingSchedule = fmap A.elems . foldl handleDuty M.empty
  where
    handleDuty m (gid,ts) = M.alter (updateGuard ts) gid m
    emptySchedule = A.listArray (0,59) (repeat 0)
    updateGuard ts = Just . applySleep ts . fromMaybe emptySchedule

applySleep [] = id
applySleep [sleep] = applySleep [sleep,60]
applySleep (sleep:wake:rest) = applySleep rest . addSleep
  where addSleep = flip (A.accum (+)) [(m,1) | m<-[sleep..wake-1]]
