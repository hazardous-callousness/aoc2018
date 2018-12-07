{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Day6 where
import           Common
import           Data.Array                    as A (accum, array, assocs,
                                                     bounds, elems, inRange,
                                                     listArray, range)
import           Data.Foldable                 (toList)
import           Data.Function                 (on)
import           Data.List                     (elemIndex, maximumBy, sortBy)
import           Data.Map.Strict               as M (alter, assocs, empty)
import           Data.Maybe                    (fromMaybe)
import           Data.Set                      as S (fromList, notMember)
import           Text.ParserCombinators.Parsec

-- 1, 1
-- 1, 6
parseInput = parse file ""
  where
    file = line `endBy` many (oneOf ", \n")
    line = do
      spaces
      x<-readInt
      char ','
      spaces
      y<-readInt
      pure (x,y)

exercise1 = solution . makeArray . eitherError . parseInput
  where
    solution arr = maximum $ fmap snd $ filter ((`notMember` infinities arr) . fst) $ countMultiples $ A.elems arr
    infinities = S.fromList . fmap snd . atBounds
    makeArray cs = A.array bounds [(c,if unique ds then fst $ head ds else 0) | c<-range bounds,let ds = distances c points]
      where
        bounds = ((2*xmin-xmax, 2*ymin-ymax),(2*xmax-xmin, 2*ymax-ymin))
        [xmin,ymin,xmax,ymax] = bounded cs
        points = zip [1..] cs
        unique ((_,d1):(_,d2):_) = d1 /= d2

exercise2 = solution . makeArray . eitherError . parseInput
  where
    solution = sum . A.elems
    makeArray cs = A.array bounds [(c,if sum ds < 10000 then 1 else 0) | c<-range bounds, let ds = fmap (distance c) cs]
      where
        bounds = ((xmin-200, ymin-200),(xmax+200, ymax+200)) -- max distance to any point is 10000/50=200 (cheaty)
        [xmin,ymin,xmax,ymax] = bounded cs



bounded cs = fmap ($ cs) [minimum . fmap fst, minimum . fmap snd, maximum . fmap fst, maximum . fmap snd]

distances c ps = sortBy (compare `on` snd) [(p,distance c c') | (p,c')<-ps]

-- manhattan distance is NOT max distance! I lost far too much time because of that!
distance (x,y) (x',y') = (+) (abs (x - x')) (abs (y - y'))

atBounds arr = (if thin then id else filter (not . within . fst)) $ A.assocs arr
  where
    thin = xmin+1 >= xmax || ymin+1 >= ymax
    ((xmin,ymin),(xmax,ymax)) = A.bounds arr
    within = A.inRange ((xmin+1,ymin+1),(xmax-1,ymax-1))

countMultiples = M.assocs . foldl updateMap M.empty
  where
    updateMap m x = M.alter (Just . (+1) . fromMaybe 0) x m
