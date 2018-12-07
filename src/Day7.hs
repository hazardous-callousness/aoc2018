{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Day7 where
import           Common
import           Data.Function                 (on)
import           Data.List                     (minimumBy, sort)
import qualified Data.Map                      as M
import           Data.Maybe                    (fromJust)
import qualified Data.Set                      as S
import           Text.ParserCombinators.Parsec

-- Step C must be finished before step A can begin.
parseInput = parse file ""
  where
    file = endBy line (many (oneOf ", \n"))
    line = do
      string "Step "
      former<-letter
      string " must be finished before step "
      latter<-letter
      string " can begin."
      pure (former,latter)



exercise1 = solution . eitherError . parseInput
  where
    solution = solve . dependencies
    solve m
      | M.null m = []
      | otherwise = letter : solve (updateMap m)
      where
        (letter,_) = M.findMin $ M.filter S.null m
        updateMap = M.map (S.delete letter) . M.delete letter

-- surely it can be smaller than this?
exercise2 = solution . eitherError . parseInput
  where
    solution = simulate 0 M.empty . dependencies
    simulate time working todo
      | M.null todo   = time + maximum (M.elems working)
      | taskAvailable = let
        (letter,_) = M.findMin availableLetters
        timeReq    = fromJust $ lookup letter $ zip ['A'..] [61..]
        working'   = M.insert letter timeReq working
        todo'      = M.delete letter todo
        in simulate time working' todo'
      | otherwise     = let
        (letter,passed) = minimumBy (compare `on` snd) $ M.assocs working
        time'           = time+passed
        working'        = M.map (subtract passed) $ M.delete letter working
        todo'           = M.map (S.delete letter) todo
        in simulate time' working' todo'
      where
        availableLetters = M.filter S.null todo
        taskAvailable    = M.size working < 5 && not (M.null availableLetters)


dependencies input = let
  allLetters     = S.fromList . uncurry (++) . unzip
  addDep m (a,b) = M.alter (fmap (S.insert a)) b m
  letters        = M.fromSet (const S.empty) $ allLetters input
  in foldl addDep letters input
