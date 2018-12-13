{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Common
    (
    module Common
    ) where
import           Control.Applicative           (liftA2)
import qualified Data.List                     as L
import qualified Data.Map.Strict               as M
import qualified Data.Set                      as S
import           Text.ParserCombinators.Parsec (digit, many1, oneOf)

-- simple wrapper for running an exercise on a given file
doExercise input ex = ex <$> readFile input


-- convert an Either (e.g. parse result) to its Right or crash badly
eitherError :: Show a => Either a b -> b
eitherError (Left a)  = error $ show a
eitherError (Right b) = b

both :: (a->b) -> (a,a) -> (b,b)
both f (a1,a2) = (f a1, f a2)

-- short-circuiting length function
hasLength :: (Num b, Eq b) => b -> [a] -> Bool
hasLength 0 []     = True
hasLength 0 (_:_)  = False
hasLength n []     = False
hasLength n (_:xs) = hasLength (n-1) xs

-- parse digits and convert to Int
readInt = (read::String->Int) <$> many1 (oneOf ('-':['0'..'9']))

-- collect list per n items
groupPer :: Int -> [a] -> [[a]]
groupPer _ [] = []
groupPer n xs = first : groupPer n rest
  where
    (first,rest) = splitAt n xs

-- produces all cycled versions of a list
-- for infinite lists it's effectively Data.List.tails
shifts :: [a] -> [[a]]
shifts xs = shifts' xs []
  where
    shifts' (x:xs) ys = ((x:xs)++ys) : shifts' xs (ys++[x])

-- get every other element of a list
evens :: [a] -> [a]
evens (x:_:xs) = x : evens xs
evens _        = []

-- test whether a list contains a duplicate element
hasDuplicate :: (Ord a) => [a] -> Bool
hasDuplicate = hasDuplicate' S.empty
  where
    hasDuplicate' s []     = False
    hasDuplicate' s (x:xs) = S.member x s || hasDuplicate' (S.insert x s) xs


--
hasBackLink :: (Foldable f, Ord k) => M.Map k (f k) -> k -> Bool
hasBackLink m k = let
  backLookup k' = case M.lookup k' m of
    Just xs -> k `elem` xs
    _       -> False
  in case M.lookup k m of
    Just xs -> L.any backLookup xs
    _       -> False

-- test whether a graph contains an edge from a to b
hasLinkTo :: (Foldable f, Ord k) => M.Map k (f k) -> k -> k -> Bool
hasLinkTo m k' k = case M.lookup k m of
    Just xs -> k' `elem` xs
    _       -> False


instance (Num t) => Num (t,t) where
  (a,b) + (c,d) = (a+c,b+d)
  (a,b) * (c,d) = (a*c-b*d,b*c+a*d)
  abs (a,b) = (abs a, abs b)
  signum (a,b) = (signum a, signum b)
  fromInteger n = (fromInteger n, fromInteger n)
  negate (a,b) = (-a,-b)
