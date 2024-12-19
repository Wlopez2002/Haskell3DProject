module Physics3D where

import Base3D

{-
This file contains all the needed code for physics, collisions, and
interactions.
-}

-- A box that defines colision. It holds a function
-- Player -> Player in case that is needed.
data CollisionBox = CollisionBox {
    canEnter :: Bool,
    func :: Player -> Player,
    p1 :: DPoint,
    p2 :: DPoint,
    p3 :: DPoint,
    p4 :: DPoint,
    p5 :: DPoint,
    p6 :: DPoint,
    p7 :: DPoint,
    p8 :: DPoint
}

-- Does not handle diagonal colliders, for now those will have to be manualy made.
boxAroundPoint :: Bool -> (Player -> Player) -> DPoint -> Float -> Float -> Float -> CollisionBox
boxAroundPoint c f (DPoint x y z) w h l = 
    (CollisionBox
        c
        f
        (DPoint (x + w) (y + h) (z + l))
        (DPoint (x + w) (y + h) (z - l))
        (DPoint (x + w) (y - h) (z + l))
        (DPoint (x + w) (y - h) (z - l))
        (DPoint (x - w) (y + h) (z + l))
        (DPoint (x - w) (y + h) (z - l))
        (DPoint (x - w) (y - h) (z + l))
        (DPoint (x - w) (y - h) (z - l))
    )

boxIntersects :: CollisionBox -> CollisionBox -> Bool
boxIntersects b1 b2 = (check b1xmin b1xmax b2xmin b2xmax) && 
                      (check b1ymin b1ymax b2ymin b2ymax) && 
                      (check b1zmin b1zmax b2zmin b2zmax)
    where
        (CollisionBox _ _ b1p1 b1p2 b1p3 b1p4 b1p5 b1p6 b1p7 b1p8) = b1
        (CollisionBox _ _ b2p1 b2p2 b2p3 b2p4 b2p5 b2p6 b2p7 b2p8) = b2
        (DPoint b1p1x b1p1y b1p1z) = b1p1
        (DPoint b1p2x b1p2y b1p2z) = b1p2
        (DPoint b1p3x b1p3y b1p3z) = b1p3
        (DPoint b1p4x b1p4y b1p4z) = b1p4
        (DPoint b1p5x b1p5y b1p5z) = b1p5
        (DPoint b1p6x b1p6y b1p6z) = b1p6
        (DPoint b1p7x b1p7y b1p7z) = b1p7
        (DPoint b1p8x b1p8y b1p8z) = b1p8

        (DPoint b2p1x b2p1y b2p1z) = b2p1
        (DPoint b2p2x b2p2y b2p2z) = b2p2
        (DPoint b2p3x b2p3y b2p3z) = b2p3
        (DPoint b2p4x b2p4y b2p4z) = b2p4
        (DPoint b2p5x b2p5y b2p5z) = b2p5
        (DPoint b2p6x b2p6y b2p6z) = b2p6
        (DPoint b2p7x b2p7y b2p7z) = b2p7
        (DPoint b2p8x b2p8y b2p8z) = b2p8
        
        b1xmin = foldr1 min [b1p1x, b1p2x, b1p3x, b1p4x, b1p5x, b1p6x, b1p7x, b1p8x]
        b1xmax = foldr1 max [b1p1x, b1p2x, b1p3x, b1p4x, b1p5x, b1p6x, b1p7x, b1p8x]
        b1ymin = foldr1 min [b1p1y, b1p2y, b1p3y, b1p4y, b1p5y, b1p6y, b1p7y, b1p8y]
        b1ymax = foldr1 max [b1p1y, b1p2y, b1p3y, b1p4y, b1p5y, b1p6y, b1p7y, b1p8y]
        b1zmin = foldr1 min [b1p1z, b1p2z, b1p3z, b1p4z, b1p5z, b1p6z, b1p7z, b1p8z]
        b1zmax = foldr1 max [b1p1z, b1p2z, b1p3z, b1p4z, b1p5z, b1p6z, b1p7z, b1p8z]
        
        b2xmin = foldr1 min [b2p1x, b2p2x, b2p3x, b2p4x, b2p5x, b2p6x, b2p7x, b2p8x]
        b2xmax = foldr1 max [b2p1x, b2p2x, b2p3x, b2p4x, b2p5x, b2p6x, b2p7x, b2p8x]
        b2ymin = foldr1 min [b2p1y, b2p2y, b2p3y, b2p4y, b2p5y, b2p6y, b2p7y, b2p8y]
        b2ymax = foldr1 max [b2p1y, b2p2y, b2p3y, b2p4y, b2p5y, b2p6y, b2p7y, b2p8y]
        b2zmin = foldr1 min [b2p1z, b2p2z, b2p3z, b2p4z, b2p5z, b2p6z, b2p7z, b2p8z]
        b2zmax = foldr1 max [b2p1z, b2p2z, b2p3z, b2p4z, b2p5z, b2p6z, b2p7z, b2p8z]

        check :: Float -> Float -> Float -> Float -> Bool
        check b1min b1max b2min b2max = (b1max >= b2min) && (b2max >= b1min)