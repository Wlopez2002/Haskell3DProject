module Main where

import Graphics3D
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

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