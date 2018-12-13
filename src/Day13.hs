module Day13 where
import           Common
import           Control.Applicative        (liftA2)
import           Control.Monad.State.Strict
import           Data.Function              (on)
import qualified Data.List                  as L
import qualified Data.Map.Strict            as M
import qualified Data.Set                   as S
import           Debug.Trace                (traceM)

type Coord = (Int,Int)
type Vel = Coord
data Turn = TLeft | TStraight | TRight
  deriving(Eq,Ord,Enum)
data Track = Junction | Piece Coord Coord
  deriving(Eq,Show)
data Cart = Cart Coord Vel Turn
  deriving(Eq,Ord)

instance Show Turn where
  show TLeft     = "l"
  show TStraight = "a"
  show TRight    = "r"

instance Show Cart where
  show (Cart c v d) = let
    getDir (1,0)  = ">"
    getDir (0,1)  = "v"
    getDir (-1,0) = "<"
    getDir (0,-1) = "^"
    in show c ++ " " ++ getDir v

parseTracks :: String -> M.Map Coord Track
parseTracks = M.fromList . scan . fmap replaceCorners . lines
  where
    -- make parsing of corners easier by labelling their direction 1-4
    -- NOTE: fails on vertical S-bends
    --  |
    -- //
    -- |
    replaceCorners xs = fmap repl $ take (length xs) $ take 3 <$> L.tails (' ':cycle xs)
      where
        repl [p,'/',n]
          | p `elem` " |^V" || n `elem` "-+<>" = '1'
          | p `elem` "-+<>" || n `elem` " |^V" = '3'
          | otherwise = error $ "hard to decypher "++[p,'/',n]
        repl [p,'\\',n]
          | p `elem` "-+<>" || n `elem` " |^V" = '2'
          | p `elem` " |^V" || n `elem` "-+<>" = '4'
          | otherwise = error $ "hard to decypher "++[p,'\\',n]
        repl [_,c,_] = c
    scan ls = [((x,y),track x y c) |(y,line)<-zip [0..] ls, (x,c)<-zip [0..] line, c `elem` "|^v-<>+1234"]
    track x y c
      | c `elem` "|^v"  = Piece top bottom
      | c `elem` "-<>"  = Piece left right
      | c == '+' = Junction
      | c == '1' = Piece right bottom
      | c == '2' = Piece bottom left
      | c == '3' = Piece left top
      | c == '4' = Piece top right
        where
          left = (x-1,y)
          right = (x+1,y)
          top = (x,y-1)
          bottom = (x,y+1)

parseCarts :: String -> [Cart]
parseCarts = scan . lines
  where
    scan ls = [Cart (x,y) (vel c) TLeft | (y,line)<-zip [0..] ls, (x,c)<-zip [0..] line, c `elem` "^v<>"]
    vel 'v' = (0,1)
    vel '^' = (0,-1)
    vel '<' = (-1,0)
    vel '>' = (1,0)

exercise1 ls = evalState simulate carts
  where
    tracks = parseTracks ls
    carts = parseCarts ls
    simulate = do
      collisions <- step tracks
      if not $ null collisions then pure $ coord $ head collisions else simulate

exercise2 ls = evalState simulate carts
  where
    tracks = parseTracks ls
    carts = parseCarts ls
    simulate = do
      step tracks
      carts' <- get
      case carts' of
        [_] -> gets (coord . head)
        _   -> simulate



step :: M.Map Coord Track -> State [Cart] [Cart]
step tracks = do
  (deleted,carts') <- fmap (moveCarts tracks) get
  traceM $ "new positions "++show carts'
  put carts'
  modify' (L.sortOn coord)
  if not $ null deleted then traceM $ "Destroyed: " ++ show deleted else pure ()
  pure deleted


-- NOTE: not fully correct
-- only collides with carts that have moved, so collisions may happen between the wrong carts
moveCarts tracks = moveCarts' [] M.empty
  where
    moveCarts' d m (cart:carts) = let
      c = coord cart
      cart' = moveCart tracks cart
      c' = coord cart'
      in case M.lookup c m of
        Just ca -> moveCarts' (cart:ca:d) (M.delete c m) carts
        Nothing -> case M.lookup c' m of
          Just ca -> moveCarts' (cart':ca:d) (M.delete c' m) carts
          Nothing -> moveCarts' d (M.insert c' cart' m) carts
    moveCarts' d m [] = (d, M.elems m)


moveCart :: M.Map Coord Track -> Cart -> Cart
moveCart tracks (Cart c v dir) = case M.lookup c tracks of
  Just (Piece t1 t2) -> let
    toT1 = Cart t1 (t1-c) dir
    toT2 = Cart t2 (t2-c) dir
    in if t1+v == c then toT2 else toT1
  Just Junction -> let
    v' = rotate dir v
    in Cart (c+v') v' (newDir dir)

coord (Cart c _ _) = c

newDir TLeft     = TStraight
newDir TStraight = TRight
newDir TRight    = TLeft

rotate TLeft v     = (snd v, negate $ fst v)
rotate TStraight v = v
rotate TRight v    = (negate $ snd v, fst v)
