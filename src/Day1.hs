module Day1 where
import Common
import Text.ParserCombinators.Parsec
import Data.List(inits,tails,nub,mapAccumL)
import Data.Either(fromRight)
import Control.Exception(throw)
import Data.Set(insert,member,empty)

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
