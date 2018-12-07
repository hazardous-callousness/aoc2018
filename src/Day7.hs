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



exercise1 = solve . dependencies . eitherError . parseInput
  where
    solve m
      | M.null m = []
      | otherwise = let
        letter = fst $ M.findMin $ M.filter S.null m
        m' = M.map (S.delete letter) $ M.delete letter m
        in letter : solve m'

exercise1' = fst . simulation cost workers . dependencies . eitherError . parseInput
  where
    cost    = const 1
    workers = 1

exercise2 = snd . simulation cost workers . dependencies . eitherError . parseInput
  where
    cost l  = fromJust $ lookup l $ zip ['A'..] [61..]
    workers = 5



simulation cost workers = step 0 M.empty []
  where
    step time working done todo
      -- done
      | M.null todo && M.null working = (done, time)
      -- free up completed tasks
      | not $ M.null $ M.filter (==0) working = let
        (ready,working') = M.partition (==0) working
        lettersDone      = M.keysSet ready
        todo'            = M.map (`S.difference` lettersDone) todo
        done'            = done ++ S.toList lettersDone
        in step time working' done' todo'
      -- assign work
      | freeWorkers > 0 && not (S.null availableLetters) = let
        newWork  = M.fromSet cost $ sTake freeWorkers availableLetters
        working' = M.union working newWork
        todo'    = M.difference todo newWork
        in step time working' done todo'
      -- go to next time step
      | otherwise = let
        passed   = minimum $ M.elems working
        time'    = time+passed
        working' = M.map (subtract passed) working
        in step time' working' done todo
      where
        freeWorkers = workers - M.size working
        availableLetters = M.keysSet $ M.filter S.null todo



dependencies input = let
  addDep m (a,b) = M.alter (fmap (S.insert a)) b m
  letters        = (M.fromSet (const S.empty) . S.fromList . uncurry (++) . unzip) input
  in foldl addDep letters input

-- modern haskell has S.take
sTake n = S.fromList . take n . S.toList
