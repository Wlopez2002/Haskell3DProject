module Main where
import Prelude hiding (init)
import Graphics3D
import Base3D
import Physics3D
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

data Game = Game {
    keys :: KeySet,
    player :: Player,
    test :: [DSquare],
    walls :: [CollisionBox]
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
        (Game k player xs w) = game -- xs needs to be sorted by average distance that way it doesn't draw whats behind
        (Player (DPoint x y z) rl ud) = player

-- Used to move the player
itter :: Float -> Game -> Game
itter dt (Game k p t w) = 
    -- TODO add a check for if we can enter the collider
    -- there is also the issue where it freezes when it collides
    -- I think this needs to be moved into movePlayer to eat movement
    -- that would trigger then collider.
    if and (map (boxIntersects pcol) w)
    --then (Game k (Player (DPoint (-4) 0 (-4)) rl ud) t w) 
    then (Game k (Player pp rl ud) t w) 
    else (Game k nextP (sortDSquares pp t) w)
    where 
        pcol = boxAroundPoint False id pp 0 4 0
        nextP = movePlayer p k
        (Player pp rl ud) = p

main :: IO ()
main = play window white 60 initial render handleKeys itter
    where
        wal1 = (DSquare (DPoint 0 (-10) 1) (DPoint 0 10 1) (DPoint 10 10 11) (DPoint 10 (-10) 11) blue)
        wal2 = (DSquare (DPoint 0 (-10) 1) (DPoint 0 10 1) (DPoint (-10) 10 11) (DPoint (-10) (-10) 11) red)
        wal3 = (DSquare (DPoint 0 10 1) (DPoint 10 10 11) (DPoint 0 10 21) (DPoint (-10) 10 11) yellow)
        collid = 
            (CollisionBox
                False
                id
                (DPoint 0 10 1) (DPoint 0 (-10) 1) (DPoint 10 10 11) (DPoint 10 (-10) 11)
                (DPoint 0 10 21) (DPoint (-10) 10 11) (DPoint 0 (-10) 21) (DPoint (-10) (-10) 11)
            )
        initial = (Game emptyKeySet (Player (DPoint 0 0 0) 0 0) [wal3, wal2, wal1] [collid])
        --initial = (Game emptyKeySet (Player (DPoint 0 0 0) 0 0) [wal3])


-- Takes a point and moves it based on a KeySet.
-- TODO account for rotation of player when moving in a direction.
movePlayer :: Player -> KeySet -> Player
movePlayer (Player (DPoint x y z) ro ra) (KeySet u d l r f b rl rr ru rd)
    = (Player (DPoint x' y' z') ro' ra')
        where 
            moveu t = if (t) then (0.5) else 0
            moved t = if (t) then (-1 * 0.5) else 0
            moveru t v = if (t) then checkRotate (v+(pi/64)) else v
            moverd t v = if (t) then checkRotate (v-(pi/64)) else v

            -- We will need the rotations for everything else
            ro' = moveru rr (moverd rl ro)
            -- A check is needed to ensure the player can turn completely around when looking up or down
            ra' = let ra'' =  moveru ru (moverd rd ra)
                  in if (ra'' <= (pi/2))
                    then ra''
                    else if (ra'' >= (3*pi/2))
                        then ra''
                        else ra

            -- How much the points should move by
            y'' = (moveu u) + (moved d) 
            x'' = (moveu r) + (moved l)
            z'' = (moveu f) + (moved b)

            -- Movement based off rotation 
            y' = y + y'' -- No rotation movement for y for now. Camera moves like a person.
            x' = x + (z'')*(sin ro') + (x'')*(sin (ro' + (pi/2)))
            z' = z + (z'')*(cos ro') + (x'')*(cos (ro'+ (pi/2)))

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
handleKeys (EventKey (Char 'd') Down _ _) (Game k p t w) = (Game (KeySet u d l True f b rl rr ru rd) p t w)
    where (KeySet u d l r f b rl rr ru rd) = k
handleKeys (EventKey (Char 'd') up _ _) (Game k p t w)= (Game (KeySet u d l False f b rl rr ru rd) p t w)
    where (KeySet u d l r f b rl rr ru rd) = k
handleKeys (EventKey (Char 'a') Down _ _) (Game k p t w) = (Game (KeySet u d True r f b rl rr ru rd) p t w)
    where (KeySet u d l r f b rl rr ru rd) = k
handleKeys (EventKey (Char 'a') up _ _) (Game k p t w)= (Game (KeySet u d False r f b rl rr ru rd) p t w)
    where (KeySet u d l r f b rl rr ru rd) = k
handleKeys (EventKey (Char 'w') Down _ _) (Game k p t w) = (Game (KeySet u d l r True b rl rr ru rd) p t w)
    where (KeySet u d l r f b rl rr ru rd) = k
handleKeys (EventKey (Char 'w') up _ _) (Game k p t w)= (Game (KeySet u d l r False b rl rr ru rd) p t w)
    where (KeySet u d l r f b rl rr ru rd) = k
handleKeys (EventKey (Char 's') Down _ _) (Game k p t w) = (Game (KeySet u d l r f True rl rr ru rd) p t w)
    where (KeySet u d l r f b rl rr ru rd) = k
handleKeys (EventKey (Char 's') up _ _) (Game k p t w)= (Game (KeySet u d l r f False rl rr ru rd) p t w)
    where (KeySet u d l r f b rl rr ru rd) = k
handleKeys (EventKey (Char 'z') Down _ _) (Game k p t w) = (Game (KeySet True d l r f b rl rr ru rd) p t w)
    where (KeySet u d l r f b rl rr ru rd) = k
handleKeys (EventKey (Char 'z') up _ _) (Game k p t w)= (Game (KeySet False d l r f b rl rr ru rd) p t w)
    where (KeySet u d l r f b rl rr ru rd) = k
handleKeys (EventKey (Char 'x') Down _ _) (Game k p t w) = (Game (KeySet u True l r f b rl rr ru rd) p t w)
    where (KeySet u d l r f b rl rr ru rd) = k
handleKeys (EventKey (Char 'x') up _ _) (Game k p t w)= (Game (KeySet u False l r f b rl rr ru rd) p t w)
    where (KeySet u d l r f b rl rr ru rd) = k
handleKeys (EventKey (SpecialKey KeyLeft) Down _ _) (Game k p t w) = (Game (KeySet u d l r f b True rr ru rd) p t w)
    where (KeySet u d l r f b rl rr ru rd) = k
handleKeys (EventKey (SpecialKey KeyLeft) up _ _) (Game k p t w)= (Game (KeySet u d l r f b False rr ru rd) p t w)
    where (KeySet u d l r f b rl rr ru rd) = k
handleKeys (EventKey (SpecialKey KeyRight) Down _ _) (Game k p t w) = (Game (KeySet u d l r f b rl True ru rd) p t w)
    where (KeySet u d l r f b rl rr ru rd) = k
handleKeys (EventKey (SpecialKey KeyRight) up _ _) (Game k p t w)= (Game (KeySet u d l r f b rl False ru rd) p t w)
    where (KeySet u d l r f b rl rr ru rd) = k
handleKeys (EventKey (SpecialKey KeyUp) Down _ _) (Game k p t w) = (Game (KeySet u d l r f b rl rr True rd) p t w)
    where (KeySet u d l r f b rl rr ru rd) = k
handleKeys (EventKey (SpecialKey KeyUp) up _ _) (Game k p t w)= (Game (KeySet u d l r f b rl rr False rd) p t w)
    where (KeySet u d l r f b rl rr ru rd) = k
handleKeys (EventKey (SpecialKey KeyDown) Down _ _) (Game k p t w) = (Game (KeySet u d l r f b rl rr ru True) p t w)
    where (KeySet u d l r f b rl rr ru rd) = k
handleKeys (EventKey (SpecialKey KeyDown) up _ _) (Game k p t w)= (Game (KeySet u d l r f b rl rr ru False) p t w)
    where (KeySet u d l r f b rl rr ru rd) = k
handleKeys _ g = g