{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TupleSections             #-}

module Day3 where
import           Common
import           Control.Applicative           (liftA2)
import           Control.Monad                 (guard)
import           Data.Array
import           Data.Function                 (on)
import           Data.List                     (any, tails)
import           Text.ParserCombinators.Parsec

-- #1 @ 829,837: 11x22
parseInput = parse file ""
  where
    file = endBy line (many (oneOf ", \n"))
    line = do
      char '#'
      claim<-readInt
      string " @ "
      x<-readInt
      char ','
      y<-readInt
      string ": "
      w<-readInt
      char 'x'
      h<-readInt
      pure (claim,((x,y),(w,h)))



exercise1 = length . filter (>1) . elems . solve . fmap (fmap (,1) . range . boundsOf . snd) . eitherError . parseInput
  where
    solve = foldl (accum (+)) fabric
    fabric = listArray ((0,0),(1000,1000)) (repeat 0)


exercise2 = fst . head . checkpairs . fmap (liftA2 (,) fst (boundsOf . snd)) . eitherError . parseInput
  where
    checkpairs = head . filter (\(c:cs) -> (not . any ((overlap `on` snd) c)) cs) . shifts


boundsOf ((x1,y1),(w,h)) = ((x1,y1),(x2,y2))
  where
    x2 = x1+w-1
    y2 = y1+h-1

overlap ((xmin,ymin),(xmax,ymax)) ((xmin',ymin'),(xmax',ymax')) =
  xmin <= xmax' && ymin <= ymax' && xmax >= xmin' && ymax >= ymin'
