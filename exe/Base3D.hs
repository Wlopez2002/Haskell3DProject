module Base3D where
import Graphics.Gloss

{-
This file contains all the needed code that multiple other files will
need to share. For example DPoint is needed by both Physics3D and Graphics3D.
Mostly it is definitions of data.
-}

-- TODO: DPoint and operations with DPoint should be replaced with code
--       From a linear algebra library. 
data DPoint = DPoint {x :: Float, y :: Float, z :: Float}

data Game = Game {
    keys :: KeySet,
    player :: Player,
    entities :: [Entity],
    draws :: [Rendr],
    walls :: [Collider]
}

data Player = Player {
    plocation :: DPoint,
    pHealthPoints :: Int,
    rotationLR :: Float, -- radians
    rotationUD :: Float -- radians
}

data Entity = Entity {
    elocation :: DPoint,
    eRendr :: Sprite,

    -- Behavior to move the entity within the game.
    mBehavior :: DPoint -> Game -> DPoint,

    -- Behavior to affect the game.
    wBehavior :: Game -> Game
}

data KeySet = KeySet {
    up :: Bool, down :: Bool,
    left :: Bool, right :: Bool,
    forward :: Bool, backward :: Bool,
    rotateLeft :: Bool, rotateRight :: Bool,
    rotateUp :: Bool, rotateDown :: Bool
}

-- A box that defines colision. It holds a function
-- Player -> Player in case that is needed.
data CollisionBox = CollisionBox {
    bcanEnter :: Bool,
    bfunc :: Player -> Player,
    borigin :: DPoint,
    p1 :: DPoint,
    p2 :: DPoint,
    p3 :: DPoint,
    p4 :: DPoint,
    p5 :: DPoint,
    p6 :: DPoint,
    p7 :: DPoint,
    p8 :: DPoint
}

-- Defines a circle for collision
data CollisionCircle = CollisionCircle {
    ccanEnter :: Bool,
    cfunc :: Player -> Player,
    corigin :: DPoint,
    radius :: Float
}

data Collider = ColBox CollisionBox | ColCir CollisionCircle

data Rendr = SPRT Sprite | DSQR DSquare

data Sprite = Sprite {p :: DPoint, picts :: [Picture]}

data DSquare = DSquare {sp1 :: DPoint,sp2 :: DPoint,sp3 :: DPoint,sp4 :: DPoint, c :: Color}

-- Strips the sprite from a list of entities
stripEntitySprites :: [Entity] -> [Rendr]
stripEntitySprites [] = []
stripEntitySprites (x:xs) = (SPRT (Sprite (addDP l dp) picts)) : (stripEntitySprites xs)
    where
        (Entity l s _ _) = x
        (Sprite dp picts) = s

--Rotates a point around the origin by r radians
rotateDP :: DPoint -> Float -> Float -> DPoint
rotateDP (DPoint x y z) rl ud =
    let
        x' = (x * cos(rl)) - (z * sin(rl)) -- Look left right
        z' = (z * cos(rl)) + (x * sin(rl))

        y' = (y * cos(ud)) - (z' * sin(ud)) -- Look up down
        z'' = (z' * cos(ud)) + (y * sin(ud))
        in DPoint x' y' z''

-- Gets the distance between two DPoints
getDistance :: DPoint -> DPoint -> Float
getDistance (DPoint x1 y1 z1) (DPoint x2 y2 z2) =
    sqrt((x2-x1)*(x2-x1)+(y2-y1)*(y2-y1)+(z2-z1)*(z2-z1))

-- Gets the difference between two DPoints
getDiff :: DPoint -> DPoint -> DPoint 
getDiff (DPoint x2 y2 z2) (DPoint x1 y1 z1) =
    (DPoint (x2-x1) (y2-y1) (z2-z1))

addDP :: DPoint -> DPoint -> DPoint 
addDP (DPoint x2 y2 z2) (DPoint x1 y1 z1) =
    (DPoint (x2+x1) (y2+y1) (z2+z1))