module Main where
import Prelude hiding (init)
import Graphics3D
import Base3D
import Physics3D
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

emptyKeySet :: KeySet
emptyKeySet = (KeySet False False False False False False False False False False)

window :: Display
window = InWindow ("Haskell 3D Project") (800,600) (10,10)

render :: Game -> Picture
render game = 
    let playerCoords = translate (-390) (290) $ scale 0.1 0.1 $ Text (show x ++ ", " ++ show y ++ ", " ++ show z ++ ", " ++ show rl ++ ","  ++ show ud)
        playerData = translate (-390) (280) $ scale 0.1 0.1 $ Text ("HealthL " ++ show hp)
    in Pictures [Pictures $ map (\x -> draw x player) (sortDrawElements (DPoint x y z) (xs ++ sprites)), playerCoords, playerData]
    where 
        (Game k player entities xs w) = game -- xs needs to be sorted by average distance that way it doesn't draw whats behind
        sprites = stripEntitySprites entities
        (Player (DPoint x y z) hp rl ud) = player
        

-- Used to move the player
-- Collisions sorta work, they are finicky. I think a lot of the issues are due to
-- the player collider being a box.
-- TODO: Find a way to make a circular collider with only one point and values which
-- determine it's size
itter :: Float -> Game -> Game
itter dt (Game k p e t w) = (Game k (Player (DPoint x' y' z') nhp nrl nud) e t w) 
    where 
        nextP = movePlayer p k
        (Player (DPoint nx ny nz) nhp nrl nud) = nextP
        (Player pp hp rl ud) = p
        (DPoint oldx oldy oldz) = pp
        xcol = rotateColliderH nrl (boxAroundPoint False id (DPoint nx oldy oldz) 0.5 5 0.5 10)
        ycol = rotateColliderH nrl (boxAroundPoint False id (DPoint oldx ny oldz) 0.5 5 0.5 10)
        zcol = rotateColliderH nrl (boxAroundPoint False id (DPoint oldx oldy nz) 0.5 5 0.5 10)
        x' = if (or (map (cIntersects xcol) w)) then oldx else nx
        y' = if (or (map (cIntersects ycol) w)) then oldy else ny
        z' = if (or (map (cIntersects zcol) w)) then oldz else nz

-- Takes a point and moves it based on a KeySet.
-- It is overcomplicated and could use future revisions
movePlayer :: Player -> KeySet -> Player
movePlayer (Player (DPoint x y z) hp ro ra) (KeySet u d l r f b rl rr ru rd)
    = (Player (DPoint x' y' z') hp ro' ra')
        where 
            --gravity = 0.2 disabled for now
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
            y'' = (moveu u) + (moved d) -- - gravity
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
                        
main :: IO ()
main = play window white 60 initialGame render handleKeys itter
    where
        {-
        TODO: The initial world, or map, should be read and parsed from a text file. For an entire game
        this would get verbose. Parsec would be good for this as it can parse a string and use less verbose
        syntax.
        -}
        wal1 = DSQR (DSquare (DPoint 0 (-10) 1) (DPoint 0 10 1) (DPoint 10 10 11) (DPoint 10 (-10) 11) blue)
        wal2 = DSQR (DSquare (DPoint 0 (-10) 1) (DPoint 0 10 1) (DPoint (-10) 10 11) (DPoint (-10) (-10) 11) red)
        wal3 = DSQR (DSquare (DPoint 0 10 1) (DPoint 10 10 11) (DPoint 0 10 21) (DPoint (-10) 10 11) yellow)
        floor = DSQR (DSquare (DPoint 30 (-10) 30) (DPoint 30 (-10) (-30)) (DPoint (-30) (-10) (-30)) (DPoint (-30) (-10) 30) black)

        testSprite = (Sprite (DPoint 0 0 0) [color red $ Polygon [(1000,1000),(1000,-1000),(-1000,-1000),(-1000,1000)], color green $ Polygon [(500,500),(500,-500),(-500,-500),(-500,500)]])

        collid = (ColBox
            (CollisionBox
                False
                id
                (DPoint 0 10 1)
                (DPoint 0 10 1) (DPoint 0 (-10) 1) (DPoint 10 10 11) (DPoint 10 (-10) 11)
                (DPoint 0 10 21) (DPoint (-10) 10 11) (DPoint 0 (-10) 21) (DPoint (-10) (-10) 11)
            ))

        colfloor = (ColBox
            (CollisionBox
                False
                id
                (DPoint 30 (-10) 30)
                (DPoint 30 (-10) 30) (DPoint 30 (-10) (-30)) (DPoint (-30) (-10) (-30)) (DPoint (-30) (-10) 30)
                (DPoint 30 (-11) 30) (DPoint 30 (-11) (-30)) (DPoint (-30) (-11) (-30)) (DPoint (-30) (-11) 30)
            ))

        testEnemyMB = \(DPoint x y z) g -> (DPoint x y z)
        testEnemy = (Entity (DPoint 0 1 (-10)) testSprite testEnemyMB (\x -> x))
        
        walls = [wal3, wal2, wal1, floor]
        colls = [collid,colfloor]
        entities = [testEnemy]
        initialPlayer = (Player (DPoint (0) 20 (0)) 10 0 0)
        initialGame = (Game emptyKeySet initialPlayer entities walls colls)
-- Makes a fireball entity. it moves in the direction
-- of the player.
--testMEFB :: Game -> Game

-- x left right, z forward backward, y up down, r rotate left right
-- Takes key input for the game, most just move the player.
handleKeys :: Event -> Game -> Game
handleKeys (EventKey (Char 'd') Down _ _) (Game k p e t w)   = (Game (KeySet u d l True f b rl rr ru rd) p e t w)
    where (KeySet u d l r f b rl rr ru rd) = k
handleKeys (EventKey (Char 'd') up _ _) (Game k p e t w)     = (Game (KeySet u d l False f b rl rr ru rd) p e t w)
    where (KeySet u d l r f b rl rr ru rd) = k
handleKeys (EventKey (Char 'a') Down _ _) (Game k p e t w)   = (Game (KeySet u d True r f b rl rr ru rd) p e t w)
    where (KeySet u d l r f b rl rr ru rd) = k
handleKeys (EventKey (Char 'a') up _ _) (Game k p e t w)     = (Game (KeySet u d False r f b rl rr ru rd) p e t w)
    where (KeySet u d l r f b rl rr ru rd) = k
handleKeys (EventKey (Char 'w') Down _ _) (Game k p e t w)   = (Game (KeySet u d l r True b rl rr ru rd) p e t w)
    where (KeySet u d l r f b rl rr ru rd) = k
handleKeys (EventKey (Char 'w') up _ _) (Game k p e t w)     = (Game (KeySet u d l r False b rl rr ru rd) p e t w)
    where (KeySet u d l r f b rl rr ru rd) = k
handleKeys (EventKey (Char 's') Down _ _) (Game k p e t w)   = (Game (KeySet u d l r f True rl rr ru rd) p e t w)
    where (KeySet u d l r f b rl rr ru rd) = k
handleKeys (EventKey (Char 's') up _ _) (Game k p e t w)     = (Game (KeySet u d l r f False rl rr ru rd) p e t w)
    where (KeySet u d l r f b rl rr ru rd) = k
handleKeys (EventKey (Char 'z') Down _ _) (Game k p e t w)   = (Game (KeySet True d l r f b rl rr ru rd) p e t w)
    where (KeySet u d l r f b rl rr ru rd) = k
handleKeys (EventKey (Char 'z') up _ _) (Game k p e t w)     = (Game (KeySet False d l r f b rl rr ru rd) p e t w)
    where (KeySet u d l r f b rl rr ru rd) = k
handleKeys (EventKey (Char 'x') Down _ _) (Game k p e t w)   = (Game (KeySet u True l r f b rl rr ru rd) p e t w)
    where (KeySet u d l r f b rl rr ru rd) = k
handleKeys (EventKey (Char 'x') up _ _) (Game k p e t w)     = (Game (KeySet u False l r f b rl rr ru rd) p e t w)
    where (KeySet u d l r f b rl rr ru rd) = k
handleKeys (EventKey (SpecialKey KeyLeft) Down _ _) (Game k p e t w)    = (Game (KeySet u d l r f b True rr ru rd) p e t w)
    where (KeySet u d l r f b rl rr ru rd) = k
handleKeys (EventKey (SpecialKey KeyLeft) up _ _) (Game k p e t w)      = (Game (KeySet u d l r f b False rr ru rd) p e t w)
    where (KeySet u d l r f b rl rr ru rd) = k
handleKeys (EventKey (SpecialKey KeyRight) Down _ _) (Game k p e t w)   = (Game (KeySet u d l r f b rl True ru rd) p e t w)
    where (KeySet u d l r f b rl rr ru rd) = k
handleKeys (EventKey (SpecialKey KeyRight) up _ _) (Game k p e t w)     = (Game (KeySet u d l r f b rl False ru rd) p e t w)
    where (KeySet u d l r f b rl rr ru rd) = k
handleKeys (EventKey (SpecialKey KeyUp) Down _ _) (Game k p e t w)      = (Game (KeySet u d l r f b rl rr True rd) p e t w)
    where (KeySet u d l r f b rl rr ru rd) = k
handleKeys (EventKey (SpecialKey KeyUp) up _ _) (Game k p e t w )       = (Game (KeySet u d l r f b rl rr False rd) p e t w)
    where (KeySet u d l r f b rl rr ru rd) = k
handleKeys (EventKey (SpecialKey KeyDown) Down _ _) (Game k p e t w)    = (Game (KeySet u d l r f b rl rr ru True) p e t w)
    where (KeySet u d l r f b rl rr ru rd) = k
handleKeys (EventKey (SpecialKey KeyDown) up _ _) (Game k p e t w)      = (Game (KeySet u d l r f b rl rr ru False) p e t w)
    where (KeySet u d l r f b rl rr ru rd) = k
handleKeys _ g = g