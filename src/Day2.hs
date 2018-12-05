module Day2 where
import Common
import Control.Applicative(liftA2)
import Control.Monad(guard)
import Data.List(sort,group,partition)
import Text.ParserCombinators.Parsec

parseInput :: String -> Either ParseError [String]
parseInput = parse line ""
  where
    line = endBy (many1 letter) (many (oneOf ", \n"))

exercise1 = uncurry (*) . both (length . filter id) . unzip . fmap (twosAndThrees . letterCount) . eitherError . parseInput
  where
    twosAndThrees = liftA2 (,) (elem 2) (elem 3)
    letterCount = fmap length . group . sort

exercise2 = head . solve . eitherError . parseInput
  where
    solve ls = do
      a<-ls
      b<-ls
      let ab = zip a b
      let (same,different) = partition (uncurry (==)) ab
      guard $ hasLength 1 different
      pure $ fmap fst same
