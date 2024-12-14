module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Data.List (sortBy)
import Data.Function (on)

class Renderable a where
    -- draw takes the Renderable class, the players point and produces a picture
    -- from it. A draw function for a single pount would take the absolute DPoint
    -- of the object and uses the players DPoint to create relative DPoints
    -- that can be turned into gloss Point for the screen. Note the object a must provide
    -- it's location.
    draw :: a -> Player -> Picture

data Game = Game {
    keys :: KeySet,
    player :: Player,
    test :: [DSquare]
}

data KeySet = KeySet {
    up :: Bool, down :: Bool,
    left :: Bool, right :: Bool,
    forward :: Bool, backward :: Bool,
    rotateLeft :: Bool, rotateRight :: Bool,
    rotateUp :: Bool, rotateDown :: Bool
}
emptyKeySet :: KeySet
emptyKeySet = (KeySet False False False False False False False False False False)

data Player = Player {
    location :: DPoint,
    rotationLR :: Float, -- radians
    rotationUD :: Float -- radians
}

data DPoint = DPoint {x :: Float, y :: Float, z :: Float}
instance Renderable DPoint where
    draw (DPoint x y z) (Player (DPoint px 0 pz) rl ud) = scale 1 1 $ translate px 0 $ circleSolid 2
    draw (DPoint x y z) (Player (DPoint px py pz) rl ud) = scale py py $ translate px 0 $ circleSolid 2
instance Show DPoint where
    show (DPoint x y z) = "(" ++ show x ++ "," ++ show y ++ "," ++ show z ++ ")"

data DSquare = DSquare {sp1 :: DPoint,sp2 :: DPoint,sp3 :: DPoint,sp4 :: DPoint, c :: Color}
instance Renderable DSquare where
    draw square (Player playerPoint rl ud) = 
        -- if every point is behind the player do not draw the square.
        if (isBehind rp1 && isBehind rp2 && isBehind rp3 && isBehind rp4)
            then
                -- TODO: there is an issue with squares perpendicular to the camera. it tries to draw
                -- parts of the square even when it is behind the camera.
                Polygon [(0,0)]
            else 
                color c $ Polygon [(projDP rp1), (projDP rp2), (projDP rp3), (projDP rp4)]  
                --Pictures [
                --    color c $ Polygon [(projDP rp1), (projDP rp2), (projDP rp3), (projDP rp4)],
                --    translate (-200) (-200) $ scale 0.1 0.1 $ Text (show (projDP rp1) ++ " " ++ show (projDP rp2) ++ " "++show (projDP rp3) ++ " "++show (projDP rp4)),
                --    translate (-200) (-250) $ scale 0.1 0.1 $ Text (show rp1 ++ " " ++ show rp2 ++ " " ++ show rp3 ++ " "++show rp4)
                --]

        where
            -- I need some kind of way to clip the points so they aren't at an odd depth.
            (DPoint px py pz) = playerPoint
            (DSquare sp1 sp2 sp3 sp4 c) = square
            -- The points relative to the player
            rp1 = rotateDP (getDiff sp1 playerPoint) rl ud
            rp2 = rotateDP (getDiff sp2 playerPoint) rl ud
            rp3 = rotateDP (getDiff sp3 playerPoint) rl ud
            rp4 = rotateDP (getDiff sp4 playerPoint) rl ud

             -- projects the DPoint into a 2d plane
             -- use screen dimensions/2 for weights, currently 400 and 300.
            projDP :: DPoint -> (Float, Float)
            projDP (DPoint x y z) = 
                if (z < 1) -- There are two 'odd' cases, when z is between 1 and -1 and when z is negative.
                    then if (z < 0)
                         -- Case where z is negative, I just flip the sign.
                         -- Not sure if that is actualy a good choice
                        then (400*(x/(abs z)),300*(y/(abs z)))
                         -- not sure what to do then -1 < z < 1
                         -- for now i just have it use z = 1
                        else (400*(x),300*(y))
                    else (400*(x/z),300*(y/z))

            --Rotates a point around the origin by r radians
            rotateDP :: DPoint -> Float -> Float -> DPoint
            rotateDP (DPoint x y z) rl ud =
                let
                    x' = (x * cos(rl)) - (z * sin(rl)) -- Look left right
                    z' = (z * cos(rl)) + (x * sin(rl))

                    y' = (y * cos(ud)) - (z' * sin(ud)) -- Look up down
                    z'' = (z' * cos(ud)) + (y * sin(ud))
                    in DPoint x' y' z''

            -- checks if a Dpoint is behind the player. Use for relative Dpoint
            isBehind :: DPoint -> Bool
            isBehind (DPoint x y z) = if (z <= 0) then True else False
    


-- Gets the distance between two DPoints
getDistance :: DPoint -> DPoint -> Float
getDistance (DPoint x1 y1 z1) (DPoint x2 y2 z2) =
    sqrt((x2-x1)*(x2-x1)+(y2-y1)*(y2-y1)+(z2-z1)*(z2-z1))

sortDSquares :: DPoint -> [DSquare] -> [DSquare]
sortDSquares pl sqrs = fst $ unzip $ sortBy (flip compare `on` snd) (zip sqrs (map (squareDistance pl) sqrs))

-- Gets the average distance of the points of a DSquare from some point
squareDistance :: DPoint -> DSquare -> Float
squareDistance dp (DSquare p1 p2 p3 p4 c) = ((getDistance dp p1)+(getDistance dp p2)+(getDistance dp p3)+(getDistance dp p4))/4

-- Gets the difference between two DPoints
getDiff :: DPoint -> DPoint -> DPoint 
getDiff (DPoint x2 y2 z2) (DPoint x1 y1 z1) =
    (DPoint (x2-x1) (y2-y1) (z2-z1))

window :: Display
window = InWindow ("Haskell 3D Project") (800,600) (10,10)

render :: Game -> Picture
render game = 
    let playerCoords = translate (-390) (290) $ scale 0.1 0.1 $ Text (show x ++ ", " ++ show y ++ ", " ++ show z ++ ", " ++ show rl ++ ","  ++ show ud)
    in Pictures [Pictures $ map (\x -> draw x player) xs, playerCoords]
    where 
        (Game k player xs) = game -- xs needs to be sorted by average distance that way it doesn't draw whats behind
        (Player (DPoint x y z) rl ud) = player

-- Used to move the player
itter :: Float -> Game -> Game
itter dt (Game k p t) = (Game k (movePlayer p k) (sortDSquares pp t))
    where 
        (Player pp rl ud) = p

main :: IO ()
main = do 
    let wal1 = (DSquare (DPoint 0 (-10) 1) (DPoint 0 10 1) (DPoint 10 10 11) (DPoint 10 (-10) 11) blue)
    let wal2 = (DSquare (DPoint 0 (-10) 1) (DPoint 0 10 1) (DPoint (-10) 10 11) (DPoint (-10) (-10) 11) red)
    let wal3 = (DSquare (DPoint 0 10 1) (DPoint 10 10 11) (DPoint 0 10 21) (DPoint (-10) 10 11) yellow)
    let initial = (Game emptyKeySet (Player (DPoint 0 0 0) 0 0) [wal3, wal2, wal1])
    --let initial = (Game emptyKeySet (Player (DPoint 0 0 0) 0 0) [wal3])
    play window white 60 initial render handleKeys itter

-- Takes a point and moves it based on a KeySet.
-- TODO account for rotation of player when moving in a direction.
movePlayer :: Player -> KeySet -> Player
movePlayer (Player (DPoint x y z) ro ra) (KeySet u d l r f b rl rr ru rd)
    = (Player (DPoint x' y' z') ro' ra')
        where 
            y' = moveu u (moved d y) 
            x' = moveu r (moved l x)
            z' = moveu f (moved b z) -- TODO have it move based in the rotation, IE forward moves the direction the camera is pointing
            ro' = moveru rr (moverd rl ro)
            ra' = moveru ru (moverd rd ra)
            moveu t v = if (t) then v+(0.5) else v
            moved t v = if (t) then v-(0.5) else v
            moveru t v = if (t) then checkRotate (v+(pi/64)) else v
            moverd t v = if (t) then checkRotate (v-(pi/64)) else v
            checkRotate :: Float -> Float -- makes sure v conforms to unit circle.
            checkRotate v = 
                if (v > 2*pi)
                    then (v - (2*pi))
                    else if (v < 0)
                        then ((2*pi) - v)
                        else v
            

-- x left right, z forward backward, y up down, r rotate left right
-- Takes key input for the game, most just move the player.
handleKeys :: Event -> Game -> Game
handleKeys (EventKey (Char 'd') Down _ _) (Game k p t) = (Game (KeySet u d l True f b rl rr ru rd) p t)
    where (KeySet u d l r f b rl rr ru rd) = k
handleKeys (EventKey (Char 'd') up _ _) (Game k p t)= (Game (KeySet u d l False f b rl rr ru rd) p t)
    where (KeySet u d l r f b rl rr ru rd) = k
handleKeys (EventKey (Char 'a') Down _ _) (Game k p t) = (Game (KeySet u d True r f b rl rr ru rd) p t)
    where (KeySet u d l r f b rl rr ru rd) = k
handleKeys (EventKey (Char 'a') up _ _) (Game k p t)= (Game (KeySet u d False r f b rl rr ru rd) p t)
    where (KeySet u d l r f b rl rr ru rd) = k
handleKeys (EventKey (Char 'w') Down _ _) (Game k p t) = (Game (KeySet u d l r True b rl rr ru rd) p t)
    where (KeySet u d l r f b rl rr ru rd) = k
handleKeys (EventKey (Char 'w') up _ _) (Game k p t)= (Game (KeySet u d l r False b rl rr ru rd) p t)
    where (KeySet u d l r f b rl rr ru rd) = k
handleKeys (EventKey (Char 's') Down _ _) (Game k p t) = (Game (KeySet u d l r f True rl rr ru rd) p t)
    where (KeySet u d l r f b rl rr ru rd) = k
handleKeys (EventKey (Char 's') up _ _) (Game k p t)= (Game (KeySet u d l r f False rl rr ru rd) p t)
    where (KeySet u d l r f b rl rr ru rd) = k
handleKeys (EventKey (Char 'z') Down _ _) (Game k p t) = (Game (KeySet True d l r f b rl rr ru rd) p t)
    where (KeySet u d l r f b rl rr ru rd) = k
handleKeys (EventKey (Char 'z') up _ _) (Game k p t)= (Game (KeySet False d l r f b rl rr ru rd) p t)
    where (KeySet u d l r f b rl rr ru rd) = k
handleKeys (EventKey (Char 'x') Down _ _) (Game k p t) = (Game (KeySet u True l r f b rl rr ru rd) p t)
    where (KeySet u d l r f b rl rr ru rd) = k
handleKeys (EventKey (Char 'x') up _ _) (Game k p t)= (Game (KeySet u False l r f b rl rr ru rd) p t)
    where (KeySet u d l r f b rl rr ru rd) = k
handleKeys (EventKey (Char 'q') Down _ _) (Game k p t) = (Game (KeySet u d l r f b True rr ru rd) p t)
    where (KeySet u d l r f b rl rr ru rd) = k
handleKeys (EventKey (Char 'q') up _ _) (Game k p t)= (Game (KeySet u d l r f b False rr ru rd) p t)
    where (KeySet u d l r f b rl rr ru rd) = k
handleKeys (EventKey (Char 'e') Down _ _) (Game k p t) = (Game (KeySet u d l r f b rl True ru rd) p t)
    where (KeySet u d l r f b rl rr ru rd) = k
handleKeys (EventKey (Char 'e') up _ _) (Game k p t)= (Game (KeySet u d l r f b rl False ru rd) p t)
    where (KeySet u d l r f b rl rr ru rd) = k
handleKeys (EventKey (Char 'r') Down _ _) (Game k p t) = (Game (KeySet u d l r f b rl rr True rd) p t)
    where (KeySet u d l r f b rl rr ru rd) = k
handleKeys (EventKey (Char 'r') up _ _) (Game k p t)= (Game (KeySet u d l r f b rl rr False rd) p t)
    where (KeySet u d l r f b rl rr ru rd) = k
handleKeys (EventKey (Char 'f') Down _ _) (Game k p t) = (Game (KeySet u d l r f b rl rr ru True) p t)
    where (KeySet u d l r f b rl rr ru rd) = k
handleKeys (EventKey (Char 'f') up _ _) (Game k p t)= (Game (KeySet u d l r f b rl rr ru False) p t)
    where (KeySet u d l r f b rl rr ru rd) = k
handleKeys _ g = g