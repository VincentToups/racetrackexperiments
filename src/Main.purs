module Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Matrix (Matrix, height, repeat, set, width, get, toIndexedArray)
import Data.Maybe (Maybe(..))
import Data.Array (snoc,cons,uncons,index,length,unsafeIndex,tail,replicate, sortBy, zip)
import Control.Monad.Eff.Random (randomInt, random, RANDOM)
import Graphics.Canvas (CANVAS, CanvasElement, getCanvasElementById, getCanvasWidth, getCanvasHeight, fillRect, getContext2D, setFillStyle, Rectangle)
import Data.Int (toNumber,floor)
import Data.String (joinWith)
import Data.Foldable (foldl)
import Partial.Unsafe (unsafePartial)
import Control.Monad.Rec.Class (Step(..), tailRecM)
import DOM (DOM)
import DOM.HTML.Window (requestAnimationFrame)
import DOM.HTML (window)
import DOM.HTML.Types (Window)
import Data.Tuple (Tuple(..), snd)

-- type Rectangle = { x :: Number, y :: Number, w :: Number, h :: Number }
rectangle :: Number -> Number -> Number -> Number -> Rectangle
rectangle x y w h = {
  x:x,
  y:y,
  w:w,
  h:h
  }

data Cell = Wall | Space Number 
instance showCell :: Show Cell where
  show Wall = "W"
  show (Space n) = show n
  
data Coord = Coord Int Int
instance showCoord :: Show Coord where
  show (Coord x y) = "Coord " <> (show x) <> " " <> (show y)

instance eqCoord :: Eq Coord where
  eq (Coord x1 y1) (Coord x2 y2) = (x1 == x2) && (y1 == y2)

data Dir = Up
         | UpRight
         | Right
         | DownRight
         | Down
         | DownLeft
         | Left
         | UpLeft 
instance showDir :: Show Dir where
  show Up = "Up"
  show UpRight = "UpRight"
  show Right = "Right"
  show DownRight = "DownRight"
  show Down = "Down"
  show DownLeft = "DownLeft"
  show Left = "Left"
  show UpLeft = "UpLeft"

defaultWallLocations :: (Array Coord)
defaultWallLocations = [Coord 90 24,Coord 49 24,Coord 90 23,Coord 49 23,Coord 90 22,Coord 49 22,Coord 90 21,Coord 74 21,Coord 73 21,Coord 72 21,Coord 71 21,Coord 70 21,Coord 69 21,Coord 68 21,Coord 67 21,Coord 49 21,Coord 35 21,Coord 71 20,Coord 70 20,Coord 69 20,Coord 68 20,Coord 67 20,Coord 66 20,Coord 65 20,Coord 64 20,Coord 49 20,Coord 35 20,Coord 49 19,Coord 35 19,Coord 74 18,Coord 73 18,Coord 72 18,Coord 71 18,Coord 70 18,Coord 69 18,Coord 68 18,Coord 67 18,Coord 49 18,Coord 35 18,Coord 35 17,Coord 5 17,Coord 35 16,Coord 5 16,Coord 35 15,Coord 10 15,Coord 9 15,Coord 8 15,Coord 7 15,Coord 6 15,Coord 5 15,Coord 4 15,Coord 3 15,Coord 67 14,Coord 35 14,Coord 32 14,Coord 5 14,Coord 67 13,Coord 32 13,Coord 29 13,Coord 28 13,Coord 27 13,Coord 26 13,Coord 25 13,Coord 24 13,Coord 23 13,Coord 22 13,Coord 5 13,Coord 67 12,Coord 32 12,Coord 5 12,Coord 99 11,Coord 98 11,Coord 97 11,Coord 67 11,Coord 48 11,Coord 47 11,Coord 46 11,Coord 45 11,Coord 44 11,Coord 43 11,Coord 42 11,Coord 41 11,Coord 32 11,Coord 5 11,Coord 67 10,Coord 32 10,Coord 5 10,Coord 67 9,Coord 32 9,Coord 67 8,Coord 32 8,Coord 67 7,Coord 32 7,Coord 11 7,Coord 11 6,Coord 11 5,Coord 52 4,Coord 11 4,Coord 86 3,Coord 85 3,Coord 84 3,Coord 83 3,Coord 82 3,Coord 81 3,Coord 80 3,Coord 79 3,Coord 52 3,Coord 20 3,Coord 19 3,Coord 18 3,Coord 17 3,Coord 16 3,Coord 15 3,Coord 14 3,Coord 13 3,Coord 12 3,Coord 11 3,Coord 52 2,Coord 35 2,Coord 34 2,Coord 33 2,Coord 32 2,Coord 31 2,Coord 30 2,Coord 29 2,Coord 28 2,Coord 12 2,Coord 11 2,Coord 52 1,Coord 39 1,Coord 38 1,Coord 37 1,Coord 36 1,Coord 35 1,Coord 34 1,Coord 33 1,Coord 32 1,Coord 12 1,Coord 11 1,Coord 52 0,Coord 12 0,Coord 11 0]

wallLocations :: (Matrix Cell) -> (Array Coord)
wallLocations m =
  let a = toIndexedArray m
      folder output c = case c of
        {x:x, y:y, value:(Space _)} -> output
        {x:x, y:y, value:Wall} -> cons (Coord x y) output
  in foldl folder [] a 

spaceTotal :: (Matrix Cell) -> Number 
spaceTotal m =
  let a = toIndexedArray m
      folder output c = case c of
        {x:x, y:y, value:Wall} -> output
        {x:x, y:y, value:(Space n)} -> output+n
  in foldl folder 0.0 a                  

spaceMax :: (Matrix Cell) -> Number 
spaceMax m =
  let a = toIndexedArray m
      folder output c = case c of
        {x:x, y:y, value:Wall} -> output
        {x:x, y:y, value:(Space n)} -> output `max` n
  in foldl folder 0.0 a                  


rgbPinch :: Int -> Int
rgbPinch c = min 255 $ max 0 c 

grey :: Int -> String
grey g = "rgb("<>(joinWith "," $ map (compose show rgbPinch) [g,g,g])<>")"

cellToRgb :: Cell -> Int -> String
cellToRgb Wall _ = "BlueViolet"
cellToRgb (Space n) nTurns =
  let g = floor(255.0*((toNumber nTurns)-n)/(toNumber nTurns))
  in grey g

data Player = Player Int Int Int Int Int
instance showPlayer :: Show Player where
  show (Player acc x y vx vy) = "Player "<>(show acc)<>" "
player :: Int -> Int -> Int -> Int -> Int -> Player
player acc x y vx vy =
  Player acc x y vx vy

newPlayer :: Int -> Int -> Int -> Player
newPlayer acc x y = Player acc x y 0 0

-- possiblePositions :: Player -> Matrix Cell 


indexToDir :: Int -> Dir
indexToDir 0 = Up
indexToDir 1 = UpRight
indexToDir 2 = Right
indexToDir 3 = DownRight
indexToDir 4 = Down
indexToDir 5 = DownLeft
indexToDir 6 = Left
indexToDir 7 = UpLeft
indexToDir i = indexToDir $ i `mod` 8

adjust :: Dir -> Coord -> Coord 
adjust Up (Coord x y)  = (Coord x (y-1))
adjust UpRight c = adjust Up $ adjust Right c
adjust Right (Coord x y) = (Coord (x+1) y)
adjust DownRight c = adjust Down $ adjust Right c
adjust Down (Coord x y) = (Coord x (y+1))
adjust DownLeft c = adjust Down $ adjust Left c
adjust Left (Coord x y) = (Coord (x-1) y)
adjust UpLeft c = adjust Up $ adjust Left c

allDirections :: (Array Dir)
allDirections = [Up, UpRight, Right, DownRight, Down, DownLeft, Left, UpLeft]

emptyTrack :: Int -> Int  -> Matrix Cell
emptyTrack w h = repeat w h $ Space 0.0

defaultTrack :: Matrix Cell
defaultTrack = setWalls (emptyTrack 100 25) defaultWallLocations

isOnTrack :: forall e. Matrix e -> Coord -> Boolean
isOnTrack m (Coord x y) =
  let w = width m
      h = height m
  in x >= 0 && x < w && y >= 0 && y < h

setCoord :: forall e. Matrix e -> Coord -> e -> (Maybe (Matrix e))
setCoord m (Coord x y) v =
  set x y v m

justOr :: forall e. Maybe e -> e -> e
justOr Nothing val = val
justOr (Just val) val' = val

setWall :: Matrix Cell -> Coord -> Matrix Cell
setWall m (Coord x y) =
  justOr (set x y Wall m) m

isWall :: Matrix Cell -> Coord -> Boolean
isWall m (Coord x y) =
  case (get x y m) of
    (Just Wall) -> true
    Nothing -> true
    (Just (Space _)) -> false

isWalkable :: Matrix Cell -> Coord -> Boolean
isWalkable m coord =
  (not $ isWall m coord) && isOnTrack m coord

adjacentWalkables :: Matrix Cell -> (Array Dir) -> Coord -> (Array Coord)
adjacentWalkables m directions coord =
  let helper directions' output =
        case uncons directions' of
          Nothing -> output
          Just {head:dir, tail:directions''} ->
            let coord' = (adjust dir coord)
            in if (isWalkable m coord')
               then helper directions'' (snoc output coord')
               else helper directions'' output
  in helper directions []

reachableWalkables :: Matrix Cell -> (Array Dir) -> Coord -> Int -> (Array Coord)
reachableWalkables m dirs start 0 = []
reachableWalkables m dirs start n =
  let loop n' positions =
        if 0 == n'
           then positions
        else
          loop (n'-1) (do position <- positions
                          adjacentWalkables m dirs position)
  in loop n [start]

selectOne :: forall e el. (Array el) -> Eff (random :: RANDOM | e) (Maybe el)
selectOne ar = 
  do i <- randomInt 0 ((length ar) - 1)
     pure $ index ar i

randomNext :: forall e. Matrix Cell -> Coord -> Eff (random :: RANDOM | e) (Maybe Coord)
randomNext m c = 
  let neighbors = adjacentWalkables m allDirections c
  in selectOne neighbors

rightMost :: Coord -> Coord -> Ordering
rightMost (Coord x1 y1) (Coord x2 y2) =
  if x1 > x2
  then LT
  else if x1 < x2
       then GT
       else EQ

randomNumbers :: forall e. Int -> Eff (random :: RANDOM | e) (Array Number)
randomNumbers n = 
  tailRecM go (Tuple n [])
  where
    go (Tuple n' numbers) =
      if n' == 0
      then pure $ Done numbers
      else
        do aNumber <- random
           pure $ Loop (Tuple (n'-1) (snoc numbers aNumber))

lessThanOrdering :: Number -> Number -> Ordering
lessThanOrdering a b =
  if a < b
  then LT
  else if a > b
       then GT
       else EQ

shuffle :: forall e x. (Array x) -> Eff (random :: RANDOM | e) (Array x)
shuffle a =
  do decorations <- randomNumbers $ length a
     pure $ snds $ sortBy byFst $ zip decorations a
  where
    snds = map snd
    byFst (Tuple q _) (Tuple r _) = lessThanOrdering q r        
                                                     
rightMostNext :: forall e. Matrix Cell -> Coord -> Eff (random :: RANDOM | e) (Maybe Coord)
rightMostNext m c = 
  let neighbors = adjacentWalkables m allDirections c
  in case length neighbors of
    0 -> pure Nothing
    n -> do neighbors' <- shuffle neighbors
            pure $ (Just (unsafePartial $ unsafeIndex (sortBy rightMost neighbors') 0))

type SimState = { track::(Matrix Cell), players::(Array Coord)}

updateBySimulatingOnce :: forall e. (Matrix Cell -> Coord -> Eff (random :: RANDOM | e) (Maybe Coord)) -> SimState -> Eff (random :: RANDOM | e) SimState
updateBySimulatingOnce next simState = 
  let loop {state:state, oldPlayers:oldPlayers, newPlayers:newPlayers} =
        case (length oldPlayers) of
          0 -> pure $ Done {track:state, players:newPlayers}
          _ ->
            let
              position = (unsafePartial $ unsafeIndex oldPlayers 0)              
            in case tail oldPlayers of
              Nothing -> pure $ Done {track:state, players:newPlayers}
              (Just rest) -> do
                selection <- next state position
                case selection of
                  (Just newPosition) -> pure $ Loop {state:(incrCoord state newPosition), oldPlayers:rest, newPlayers:(snoc newPlayers newPosition)}
                  Nothing -> pure $ Loop {state:state, oldPlayers:rest, newPlayers:(snoc newPlayers position)}                
  in tailRecM loop {state:simState.track, oldPlayers:simState.players, newPlayers:[]}

updateBySimulating :: forall e. (Matrix Cell -> Coord -> Eff (random :: RANDOM | e) (Maybe Coord)) -> SimState -> Int -> Eff (random :: RANDOM | e) SimState 
updateBySimulating next simState n = 
  tailRecM go {simState:simState, n: n}
  where
    go {simState:simState', n:0} = pure $ Done simState'
    go {simState:simState', n:n'} = do
      simState'' <- (updateBySimulatingOnce next simState')
      pure $ Loop {simState:simState'', n:(n'-1)}

updateBySimulatingRandomMotion :: forall e. SimState -> Int -> Eff (random :: RANDOM | e) SimState 
updateBySimulatingRandomMotion = updateBySimulating randomNext

updateBySimulatingRtRandomMotion :: forall e. SimState -> Int -> Eff (random :: RANDOM | e) SimState 
updateBySimulatingRtRandomMotion = updateBySimulating rightMostNext


makeSimulationState :: Matrix Cell -> Int -> Coord -> SimState
makeSimulationState m n position =
  { track:m, players:(replicate n position) }

setWalls :: Matrix Cell -> (Array Coord) -> Matrix Cell
setWalls m ac =
  foldl (\ m' (Coord x y) -> (justOr (set x y Wall m') m')) m ac

incrCoordinates :: Matrix Cell -> (Array Coord) -> Matrix Cell
incrCoordinates m c =
  let f m' (Coord x y) = case (get x y m') of
        Just Wall -> m'
        Just (Space current) -> (justOr (set x y (Space (current + 1.0)) m') m')
        Nothing -> m'
  in foldl f m c

incrCoord :: Matrix Cell -> Coord -> Matrix Cell
incrCoord m (Coord x y) =
  case (do cell <- (get x y m)
           case cell of
             Wall -> (Just m)
             (Space val) -> set x y (Space (1.0 + val)) m) of
    (Just m') -> m'
    Nothing -> m

wallRun :: Matrix Cell -> Dir -> Int -> Coord -> Matrix Cell
wallRun map dir 0 coord = map
wallRun m dir n coord =
  if (isOnTrack m coord)
    then case (setCoord m coord $ Wall) of
      (Just m') -> wallRun m' dir (n - 1) (adjust dir coord)
      Nothing  -> m
    else m

-- openNeighbors :: Matrix Cell -> Coord -> (Array Dir) -> Int -> (Array Coord)
-- openNeighbors m start directions 0 = [start]
-- openNeighbors m start [] n = [start]
-- openNeighbors m start directions n =
--   let helper m' start' directions' n' results =
--         case n' of
--           0 -> snoc start' results
--           n -> do dir <- directions
                  


randomDirection :: forall e. Eff (random :: RANDOM | e) Dir
randomDirection = do
  i <- randomInt 0 3
  pure $ indexToDir i

randomWalls :: forall e. Matrix Cell -> Int -> Int -> Int -> Eff (random :: RANDOM | e) (Matrix Cell)
randomWalls theMap nWalls minLength maxLength = 
  case nWalls of
    0 -> pure theMap
    n -> do
      dir <- randomDirection
      len <- randomInt minLength maxLength
      x <- randomInt 0 ((width theMap) - 1)
      y <- randomInt 0 ((height theMap) - 1)
      randomWalls (wallRun theMap dir len (Coord x y)) (nWalls - 1) minLength maxLength

randomlyTransformMatrixElements :: forall e el. (Int -> Int -> el -> Eff (random :: RANDOM | e) el) -> Matrix el -> Eff (random :: RANDOM | e) (Matrix el)
randomlyTransformMatrixElements tr mt =
  let w = width mt
      h = height mt
      loop x y m = do
        if (y == h)
          then pure m
          else do let x' = ((x+1) `mod` w)
                      y' = if x' == 0 then (y + 1) else y
                  case (get x y m) of
                    (Just el) -> do nv <- (tr x y el)
                                    case (set x y nv m) of
                                      (Just m') -> loop x' y' m'
                                      Nothing -> loop x' y' m
                    Nothing -> loop x' y' m
  in loop 0 0 mt

randomizeSpaces :: forall e. Matrix Cell -> Int -> Int -> Eff (random :: RANDOM | e) (Matrix Cell)
randomizeSpaces mt mn mx = 
  let f x y p = case p of
        Wall -> pure p
        Space n -> do n' <- randomInt mn mx
                      pure $ Space $ toNumber $ n'
  in randomlyTransformMatrixElements f mt

randoms :: forall e. Int -> Eff (random :: RANDOM | e) (Array Number) 
randoms n =
  let randomHelper nh a =
        case nh of
          0 -> pure a
          nhh -> do
                 r <- random
                 randomHelper (nhh - 1) $ snoc a r
  in randomHelper n []

forEachMatrixElement :: forall e el. (Int -> Int -> el -> Eff (canvas :: CANVAS | e) Unit) -> Matrix el -> Eff (canvas :: CANVAS | e) Unit
forEachMatrixElement f m =
  let w = width m
      h = height m
      getAp a b r g = case (get a b r) of
        (Just v) -> g v
        Nothing  -> pure unit
      loop x y = do
        if (y == h)
          then pure unit
          else do getAp x y m (f x y)
                  let x' = ((x+1) `mod` w)
                      y' = if x' == 0 then (y + 1) else y
                  loop x' y'
  in loop 0 0



           
drawMap :: forall e. Matrix Cell -> CanvasElement -> Eff (canvas :: CANVAS | e) Unit
drawMap theMap can = 
  do w <- getCanvasWidth can
     h <- getCanvasHeight can
     let mw = toNumber $ width theMap
         mh = toNumber $ height theMap
         rw = w/mw
         rh = h/mh
         norm = spaceMax theMap
     ctx <- getContext2D can
     _ <- setFillStyle "white" ctx
     _ <- fillRect ctx (rectangle 0.0 0.0 w h)
     let each x y v = do
           _ <- setFillStyle (cellToRgb v (floor norm)) ctx 
           _ <- fillRect ctx (rectangle ((toNumber x)*rw) ((toNumber y)*rh) rw rh)
           pure unit
     forEachMatrixElement each theMap

once :: forall e. CanvasElement -> SimState -> Window -> Eff (dom :: DOM, console :: CONSOLE, random :: RANDOM, canvas :: CANVAS | e) Unit
once can ss window' = do
  simState <- updateBySimulatingRtRandomMotion ss 1
  drawMap simState.track can
  --log $ show $ simState.players 
  _ <- requestAnimationFrame (once can simState window') window'
  pure unit


main :: forall e. Eff (dom :: DOM, console :: CONSOLE, random :: RANDOM, canvas :: CANVAS | e) Unit
main = do
  can <- getCanvasElementById "vis"
  let simState = (makeSimulationState defaultTrack 100 (Coord 0 0))
  case can of
    (Just canvas) -> do
      log "Got a canvas"
      window' <- window
      _ <- requestAnimationFrame (once canvas simState window') window'
      pure unit
    Nothing -> do
      log "Got nothing."
      pure unit
      
