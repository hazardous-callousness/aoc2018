{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Common
    (
    eitherError,
    doExercise,
    both,
    hasLength,
    readInt,
    groupPer,
    shifts
    ) where
import           Text.ParserCombinators.Parsec (digit, many1)

-- simple wrapper for running an exercise on a given file
doExercise input ex = fmap ex (readFile input)


-- convert an Either (e.g. parse result) to its Right or crash badly
eitherError :: Show a => Either a b -> b
eitherError (Left a)  = (error . show) a
eitherError (Right b) = b

both :: (a->b) -> (a,a) -> (b,b)
both f (a1,a2) = (f a1, f a2)

-- short-circuiting length function
hasLength 0 []     = True
hasLength 0 (_:_)  = False
hasLength n []     = False
hasLength n (_:xs) = hasLength (n-1) xs

-- parse digits and convert to Int
readInt = fmap (read::String->Int) (many1 digit)

-- collect list per n items
groupPer _ [] = []
groupPer n xs = first : groupPer n rest
  where
    (first,rest) = splitAt n xs

-- produces all cycled versions of a list
-- for infinite lists it's effectively Data.List.tails
shifts xs = shifts' xs []
  where
    shifts' (x:xs) ys = ((x:xs)++ys) : shifts' xs (ys++[x])
