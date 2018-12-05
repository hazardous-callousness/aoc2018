module Day1 where
import Common
import Control.Applicative(liftA2)
import Control.Monad(guard)
import Data.Function(on)
import Data.List(inits,tails,mapAccumL,minimumBy)
import Data.Set(insert,member,empty)
import Text.ParserCombinators.Parsec

parseInput :: String -> Either ParseError [Int]
parseInput = parse line ""
  where
    line = endBy (posNum <|> negNum) (many (oneOf ", \n"))
    posNum = char '+' >> fmap read (many1 digit)
    negNum = char '-' >> fmap (negate . read) (many1 digit)

exercise1 = sum . eitherError . parseInput


-- Step 1: read the exercise carefully
-- Step 2: don't fuck up your algorithms
-- Step 3: use a slightly more efficient method when you're waiting too long

exercise2 = firstDuplicate . eitherError . parseInput
  where firstDuplicate = head . duplicates2 . snd . mapAccumL (\acc val -> (acc+val,acc+val)) 0 . cycle


{-
  Step 4: Use an approach that runs in a simple O(n^2) instead of whatever the
  above algorithm needs. (One could say the above requires o(2^m) with m the
  size of the numbers, while a good implementation of divMod (the slowest
  operation on var-sized numbers in this algorithm) requires O(m log m), so the
  algorithm below would still be bound to O(m^3 log m).)

  Let a_0,a_1..a_n be the frequency differences and c_i = sum a_0..a_i the
  cumulative frequencies. This also means c_n is the frequency difference for
  a full cycle.
  If a frequency repeats within one cycle, there must be a c_i = c_j for i/=j.
  If a frequency repeats after more than one cycle, there must be a
  c_i = c_j+c_n*k. So all frequency repeats are of the form
  c_i-c_j = c_n*k and their first repeat is at step (c_i-c_j)/c_n*n+j .
  Find the smallest repeat that is not a trivial case (i=j and k=0).
-}

exercise2' = fst . minimumBy (compare `on` snd) . repeats . splitLast . subSums . eitherError . parseInput
  where
    subSums = scanl (+) 0
    splitLast = liftA2 (,) init last
    repeats (cs,c_n) = do
      let n = length cs
      let indexed = zip [0..] cs
      (i,c_i)<-indexed
      (j,c_j)<-indexed
      let (cycles,diff) = (c_i-c_j) `divMod` c_n
      guard $ cycles >= 0 && diff == 0
      guard $ cycles >  0 || i /= j
      pure (c_i,cycles*n+j)

-- solve :: [Int] -> [(Int,Int)]
-- solve = filter ((==0) . snd) . map (both sum) . filter (not . null . snd) . concatMap splitEverywhere . inits
-- -- solve2 :: Foldable f => f Int -> [Int]
-- solve2 = foldl test ([0],[])
--   where
--     test (acc,out) cur = (acc',out')
--       where
--         val  = head acc
--         val' = cur + val
--         acc' = val':acc
--         out' = out ++ if val' `elem` acc then [val'] else []
--
-- duplicate xs = xs ++ xs
--
-- duplicates :: Eq a => [a] -> [a]
-- duplicates = duplicates' []
--   where
--     duplicates' found (x:xs)
--       | x `elem` found = x : duplicates' found xs
--       | otherwise      = duplicates' (x:found) xs
--     duplicates' _ [] = []

duplicates2 :: Ord a => [a] -> [a]
duplicates2 = dup empty
  where
    dup set (x:xs)
      | x `member` set = x : dup set xs
      | otherwise      = dup (insert x set) xs
    dup _ [] = []

-- both :: (a->b) -> (a,a) -> (b,b)
-- both f (a1,a2) = (f a1, f a2)
--
-- subspans :: [a] -> [[a]]
-- subspans = concatMap tails . inits
--
-- splitEverywhere xs = splitEverywhere' xs []
--   where
--     splitEverywhere' (x:xs) ys = (ys,x:xs) : splitEverywhere' xs (ys++[x])
--     splitEverywhere' [] ys = [(ys,[])]
