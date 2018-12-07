module Day5 where
import           Data.Char (isAlpha, toLower)


-- do check for non-alpha characters
exercise1 = length . solve [] . filter isAlpha
  where
    annihilates a b = sameLetter a b && a /= b
    solve [] [] = []
    solve [] (y:ys) = solve [y] ys
    solve (x:xs) (y:ys) = if x `annihilates` y then solve xs ys else solve (y:x:xs) ys
    solve xs [] = xs

exercise2 text = minimum [exercise1 $ filter (not . sameLetter l) text | l<-['a'..'z']]


sameLetter a b = toLower a == toLower b
