module Main where
import Prelude hiding (init)
import Graphics3D
import Base3D
import Data.List
import Physics3D
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

emptyKeySet :: KeySet
emptyKeySet = (KeySet False False False False False False False False False False)

window :: Display
window = InWindow ("Haskell 3D Project") (800,600) (10,10)

render :: Game -> Picture
render game = 
    let playerCoords = translate (-390) (290) $ scale 0.1 0.1 $ Text (show x ++ ", " ++ show y ++ ", " ++ show z ++ ", " ++ show rl ++ ","  ++ show ud ++ ", Entity Count: " ++ show (length entities) ++ ", Time: " ++ show gTime )
        playerData = translate (-390) (270) $ scale 0.2 0.2 $ Text ("Health: " ++ show hp)
    in Pictures [Pictures $ map (\x -> draw x player) (sortDrawElements (DPoint x y z) (xs ++ sprites)), playerCoords, playerData]
    where 
        (Game k player entities xs w gTime) = game -- xs needs to be sorted by average distance that way it doesn't draw whats behind
        sprites = stripEntitySprites entities
        (Player (DPoint x y z) hp rl ud) = player
        

-- Used to move the player
-- Collisions sorta work, they are finicky. I think a lot of the issues are due to
-- the player collider being a box.
-- TODO: Find a way to make a circular collider with only one point and values which
-- determine it's size
itter :: Float -> Game -> Game
itter dt (Game k p e t w gTime) = foldl runEGBev game' entitieAEM
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

        -- This number shouldn't get high enough to break things, but keep in mind it is never
        -- reset.
        gTime' = gTime + dt
        
        gameAPM = (Game k (Player (DPoint x' y' z') nhp nrl nud) e t w gTime') 

        entitieAEM = map (\x -> runEMBev x gameAPM) e

        game' = (Game k (Player (DPoint x' y' z') nhp nrl nud) entitieAEM t w gTime') 

        runEMBev :: Entity -> Game -> Entity
        runEMBev (Entity tid iip eloc sprite mBev lastEx gBev) g = (Entity tid iip (mBev eloc g) sprite mBev lastEx gBev) 
        runEGBev :: Game -> Entity -> Game
        runEGBev g (Entity tid iip eloc sprite mBev lastEx gBev) = gBev (Entity tid iip eloc sprite mBev lastEx gBev) g

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

        testEnemy = (Entity 0 0 (DPoint 0 10 (-10)) testSprite (\a b -> a) 0 testMEFB)
        testEnemy2 = (Entity 0 1 (DPoint (-10) 10 0) testSprite (\a b -> a) 0 testMEFB)
        
        walls = [wal3, wal2, wal1, floor]
        colls = [collid,colfloor]
        entities = [testEnemy,testEnemy2]
        initialPlayer = (Player (DPoint (0) 20 (0)) 10 0 0)
        initialGame = (Game emptyKeySet initialPlayer entities walls colls 0)

-- Makes a fireball entity. it moves in a direction
-- of the player.
-- TODO: For some reason created fireballs aren't apearing.
testMEFB :: Entity -> Game -> Game
testMEFB entity (Game keys player entities walls colls gTime) = 
    if ((lastEx + 2) < gTime)
        then (Game keys player (entities' ++ [fireball]) walls colls gTime)
        else (Game keys player entities walls colls gTime)
    where
        (Entity tid iid loc sprite mBev lastEx gBev) = entity 
        testSprite = (Sprite (DPoint 0 0 0) [color blue $ Polygon [(1000,1000),(1000,-1000),(-1000,-1000),(-1000,1000)]])
        testFBMB = \(DPoint x y z) g -> (DPoint (x+0.5) y z)
        fireball = (Entity 1 (length entities) loc testSprite testFBMB gTime testFBG)
        entities' = replace entity (Entity tid iid loc sprite mBev gTime gBev) entities

-- Game test for fireball, deletes itself if it runs into a wall.
testFBG :: Entity -> Game -> Game
testFBG entity (Game keys player entities walls colls gTime) =
    if ((lastEx + 8) < gTime)
        then if (cIntersects playerColBox fbColBox)
            then (Game keys playerHit (delete entity entities) walls colls gTime) 
            else (Game keys player (delete entity entities) walls colls gTime) 
        else if (cIntersects playerColBox fbColBox)
            then (Game keys playerHit (delete entity entities) walls colls gTime) 
            else (Game keys player entities walls colls gTime)
    where
        (Entity tid iid loc sprite mBev lastEx gBev) = entity 
        (Player (DPoint x y z) hp ro ra) = player
        playerHit = (Player (DPoint x y z) (hp-1) ro ra)
        playerColBox = rotateColliderH ro (boxAroundPoint False id (DPoint x y z) 5 5 5 0)
        fbColBox = boxAroundPoint False id loc 1 1 1 0

-- x left right, z forward backward, y up down, r rotate left right
-- Takes key input for the game, most just move the player.
handleKeys :: Event -> Game -> Game
-- Mouse
--handleKeys (EventMotion (x, y)) (Game k p e t w gt) = 
-- Keys
handleKeys (EventKey (Char 'd') Down _ _) (Game k p e t w gt)   = (Game (KeySet u d l True f b rl rr ru rd) p e t w gt)
    where (KeySet u d l r f b rl rr ru rd) = k
handleKeys (EventKey (Char 'd') up _ _) (Game k p e t w gt)     = (Game (KeySet u d l False f b rl rr ru rd) p e t w gt)
    where (KeySet u d l r f b rl rr ru rd) = k
handleKeys (EventKey (Char 'a') Down _ _) (Game k p e t w gt)   = (Game (KeySet u d True r f b rl rr ru rd) p e t w gt)
    where (KeySet u d l r f b rl rr ru rd) = k
handleKeys (EventKey (Char 'a') up _ _) (Game k p e t w gt)     = (Game (KeySet u d False r f b rl rr ru rd) p e t w gt)
    where (KeySet u d l r f b rl rr ru rd) = k
handleKeys (EventKey (Char 'w') Down _ _) (Game k p e t w gt)   = (Game (KeySet u d l r True b rl rr ru rd) p e t w gt)
    where (KeySet u d l r f b rl rr ru rd) = k
handleKeys (EventKey (Char 'w') up _ _) (Game k p e t w gt)     = (Game (KeySet u d l r False b rl rr ru rd) p e t w gt)
    where (KeySet u d l r f b rl rr ru rd) = k
handleKeys (EventKey (Char 's') Down _ _) (Game k p e t w gt)   = (Game (KeySet u d l r f True rl rr ru rd) p e t w gt)
    where (KeySet u d l r f b rl rr ru rd) = k
handleKeys (EventKey (Char 's') up _ _) (Game k p e t w gt)     = (Game (KeySet u d l r f False rl rr ru rd) p e t w gt)
    where (KeySet u d l r f b rl rr ru rd) = k
handleKeys (EventKey (Char 'z') Down _ _) (Game k p e t w gt)   = (Game (KeySet True d l r f b rl rr ru rd) p e t w gt)
    where (KeySet u d l r f b rl rr ru rd) = k
handleKeys (EventKey (Char 'z') up _ _) (Game k p e t w gt)     = (Game (KeySet False d l r f b rl rr ru rd) p e t w gt)
    where (KeySet u d l r f b rl rr ru rd) = k
handleKeys (EventKey (Char 'x') Down _ _) (Game k p e t w gt)   = (Game (KeySet u True l r f b rl rr ru rd) p e t w gt)
    where (KeySet u d l r f b rl rr ru rd) = k
handleKeys (EventKey (Char 'x') up _ _) (Game k p e t w gt)     = (Game (KeySet u False l r f b rl rr ru rd) p e t w gt)
    where (KeySet u d l r f b rl rr ru rd) = k
handleKeys (EventKey (Char 'j') Down _ _) (Game k p e t w gt)    = (Game (KeySet u d l r f b True rr ru rd) p e t w gt)
    where (KeySet u d l r f b rl rr ru rd) = k
handleKeys (EventKey (Char 'j') up _ _) (Game k p e t w gt)      = (Game (KeySet u d l r f b False rr ru rd) p e t w gt)
    where (KeySet u d l r f b rl rr ru rd) = k
handleKeys (EventKey (Char 'l') Down _ _) (Game k p e t w gt)   = (Game (KeySet u d l r f b rl True ru rd) p e t w gt)
    where (KeySet u d l r f b rl rr ru rd) = k
handleKeys (EventKey (Char 'l') up _ _) (Game k p e t w gt)     = (Game (KeySet u d l r f b rl False ru rd) p e t w gt)
    where (KeySet u d l r f b rl rr ru rd) = k
handleKeys (EventKey (Char 'i') Down _ _) (Game k p e t w gt)      = (Game (KeySet u d l r f b rl rr True rd) p e t w gt)
    where (KeySet u d l r f b rl rr ru rd) = k
handleKeys (EventKey (Char 'i') up _ _) (Game k p e t w gt)        = (Game (KeySet u d l r f b rl rr False rd) p e t w gt)
    where (KeySet u d l r f b rl rr ru rd) = k
handleKeys (EventKey (Char 'k') Down _ _) (Game k p e t w gt)    = (Game (KeySet u d l r f b rl rr ru True) p e t w gt)
    where (KeySet u d l r f b rl rr ru rd) = k
handleKeys (EventKey (Char 'k') up _ _) (Game k p e t w gt)      = (Game (KeySet u d l r f b rl rr ru False) p e t w gt)
    where (KeySet u d l r f b rl rr ru rd) = k
handleKeys _ g = g
