{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Day7 where
import           Common
import           Control.Monad
import           Text.ParserCombinators.Parsec

data Tree a = Tree [Tree a] [a]


parseInput = parse file ""
  where
    file = endBy tree (many (oneOf ", \n"))
    tree = do
      ccount <- readInt
      spaces
      mcount <- readInt
      spaces
      children <- replicateM ccount tree
      meta <- replicateM mcount (spaces >> readInt)
      spaces
      pure (Tree children meta)

exercise1 = sum . treeToList . head . eitherError . parseInput

exercise2 = indexedSum . head . eitherError . parseInput



treeToList (Tree ts ms) = concatMap treeToList ts ++ ms

indexedSum (Tree [] ms) = sum ms
indexedSum (Tree ts ms) = sum $ fmap sub ms
  where
    len = length ts
    sub i = if i == 0 || i > len then 0 else indexedSum (ts !! (i-1))
