{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Day10 where
import           Common
import           Data.Array
import           Data.Foldable                 (foldl')
import           Data.Function                 (on)
import           Data.List                     (groupBy, maximumBy, minimumBy,
                                                sortOn, transpose)
import           Text.ParserCombinators.Parsec

-- position=< 10775, -31651> velocity=<-1,  3>
parseInput = parse file ""
  where
    file = line `endBy` many (oneOf ", \n")
    coord = do
      string "<"
      spaces
      x<-readInt
      string ","
      spaces
      y<-readInt
      string ">"
      pure (x,y)
    line = do
      string "position="
      pos<-coord
      string " velocity="
      vel<-coord
      pure (pos,vel)

readInteg = fmap (read::String->Integer) (many1 (oneOf ('-':['0'..'9'])))

exercise1 = toAscii . solve . eitherError . parseInput
  where
    solve ps = let
      getY ((_,y),_) = y
      ((_,miny),(_,maxvy)) = minimumBy (compare `on` getY) ps
      ((_,maxy),(_,minvy)) = maximumBy (compare `on` getY) ps
      dy = maxy-miny
      vy = maxvy-minvy
      boundSize = rangeSize . boundsOf . (\t -> fmap (pointAt t) ps)
      times = if vy /= 0 then [(dy-10) `div` vy .. (dy+10) `div` vy] else [0]
      time = minimumBy (compare `on` boundSize) times
      in fmap (pointAt time) ps



bigboy1 = bigboy 50 500

bigboy iGroupSize boundSize = fmap toAscii' . solve . eitherError . parseInput
  where
    solve xs = (fmap (viewPoints xs) . isectBounds . isectGroups) xs
    viewPoints xs (t,bs) = let
        bs' = increaseBounds bs
        arr = array bs' [(i,'.') | i<-range bs']
        points = fmap (\c->(c,'#')) . filter (inRange bs') . fmap (pointAt t)
      in arr // points xs
    isectBounds = filter ((<boundSize) . rangeSize . snd) . fmap toBounds . filter ((>iGroupSize) . length)
    toBounds group = (fst $ head group, boundsOf $ fmap snd group)
    isectGroups = groupBy fstEq . sortOn fst . filter ((>0) . fst) . intersects
    intersects xs = [intersection x y | x<-xs, y<-xs]

toAscii cs = unlines $ transpose $ groupPer (fromIntegral $ snd (snd bound) - snd (fst bound) + 1) $ elems arr
  where
    bound = boundsOf cs
    arr = array bound [(i,'.') | i<-range bound] // zip cs (repeat '#')

toAscii' arr = unlines $ transpose $ groupPer (fromIntegral $ ymax - ymin + 1) $ elems arr
  where
    ((_,ymin),(_,ymax)) = bounds arr


boundsOf :: (Ord t2, Ord t1) => [(t1, t2)] -> ((t1, t2), (t1, t2))
boundsOf cs = foldl' minMax (head cs, head cs) cs
  where
    minMax ((xmin,ymin),(xmax,ymax)) (x,y) = let
      xmin' = min xmin x
      ymin' = min ymin y
      xmax' = max xmax x
      ymax' = max ymax y
      in ((xmin',ymin'),(xmax',ymax'))


pointAt t ((x,y),(dx,dy)) = (x+dx*t,y+dy*t)

increaseBounds ((xmin,ymin),(xmax,ymax)) = ((xmin-1,ymin-1),(xmax+1,ymax+1))

intersection ((x1,y1),(vx1,vy1)) ((x2,y2),(vx2,vy2)) = let
    det = vx2*vy1-vy2*vx1
    time = (vx1*(y2-y1)+vy1*(x1-x2)) `div` det
  in if det == 0 then (0,(0,0)) else (time,(x2+vx2*time,y2+vy2*time))

fstEq (a,_) (b,_) = a==b
