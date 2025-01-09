module Graphics3D where

import Base3D
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Data.List (sortBy)
import Data.Function (on)

{-
This file contains all the needed code to handle graphics.
-}

class Renderable a where
    -- draw takes the Renderable class, the players point and produces a picture
    -- from it. A draw function for a single pount would take the absolute DPoint
    -- of the object and uses the players DPoint to create relative DPoints
    -- that can be turned into gloss Point for the screen. Note the object a must provide
    -- it's location.
    draw :: a -> Player -> Picture

-- Render code for the top level Rendr object.
instance Renderable Rendr where
    draw (SPRT x) = draw x
    draw (DSQR x) = draw x

instance Show DPoint where
    show (DPoint x y z) = "(" ++ show x ++ "," ++ show y ++ "," ++ show z ++ ")"

-- Render code for sprites
instance Renderable Sprite where
    draw s (Player playerPoint hp rl ud) = 
        if (isBehind (DPoint rx ry rz))
            then
                Polygon [(0,0)]
            else
                Pictures (map (\x -> translate fx fy $ scale (1/rz) (1/rz) x) picts)
        where
            (DPoint px py pz) = playerPoint
            (Sprite sp picts) = s
            sc = getDiff sp playerPoint

            (fx,fy) = projDP (DPoint rx ry rz)

            (DPoint rx ry rz) = rotateDP sc rl ud

-- Render code for DSquares.
-- TODO: This code is still a glitchy
instance Renderable DSquare where
    draw square (Player playerPoint hp rl ud) = 
        -- if every point is behind the player do not draw the square.
        if (isBehind rp1 && isBehind rp2 && isBehind rp3 && isBehind rp4)
            then
                Polygon [(0,0)]
            else 
                color c $ Polygon [(projDP rp1), (projDP rp2), (projDP rp3), (projDP rp4)]  
        where
            -- I need some kind of way to clip the points so they aren't at an odd depth.
            (DPoint px py pz) = playerPoint
            (DSquare sp1 sp2 sp3 sp4 c) = square
            -- The points relative to the player
            rp1 = rotateDP (getDiff sp1 playerPoint) rl ud
            rp2 = rotateDP (getDiff sp2 playerPoint) rl ud
            rp3 = rotateDP (getDiff sp3 playerPoint) rl ud
            rp4 = rotateDP (getDiff sp4 playerPoint) rl ud

-- Moves a DSquare by a DPoint
translateDSquare :: DSquare -> DPoint -> DSquare
translateDSquare square point = (DSquare (addDP point sp1) (addDP point sp2) (addDP point sp3) (addDP point sp4) c)
    where
        (DSquare sp1 sp2 sp3 sp4 c) = square

-- projects the DPoint into a 2d plane
-- use screen dimensions/2 for weights, currently 400 and 300.
projDP :: DPoint -> (Float, Float)
projDP (DPoint x y z) = 
    if (z < 1) -- There are two 'odd' cases, when z is between 1 and -1 and when z is negative.
        then if (z < 0)
            -- Case where z is negative, mult instead of
            -- div, and use abs z.
            then (400*(x*(abs z)),300*(y*(abs z)))
            -- not sure what to do then -1 < z < 1
            -- for now i just have it use z = 1
            else (400*(x/z),300*(y/z))
        else (400*(x/z),300*(y/z))

 -- checks if a Dpoint is behind the player. Use for relative DPoint
isBehind :: DPoint -> Bool
isBehind (DPoint x y z) = if (z <= 0) then True else False


{-
TODO: While this works it leads to issues, especialy with floors, as other objects may
be on average closer to the player when they should be further. Instead of averaging
the distancesfor DSquares find if it's distance based on the actual plane.
-}
-- Sorts Rendr Elements by their distance to a DPoint.
sortDrawElements :: DPoint -> [Rendr] -> [Rendr]
sortDrawElements pl s = fst $ unzip $ sortBy (flip compare `on` snd) (map (h pl) s)
    where
        h :: DPoint -> Rendr -> (Rendr, Float)
        h pp (DSQR sqr) = (DSQR sqr, squareDistance pp sqr)
        h pp (SPRT (Sprite sp picts)) = (SPRT (Sprite sp picts), getDistance pp sp)

-- Gets the average distance of the points of a DSquare from some point
squareDistance :: DPoint -> DSquare -> Float
squareDistance dp (DSquare p1 p2 p3 p4 c) = ((getDistance dp p1)+(getDistance dp p2)+(getDistance dp p3)+(getDistance dp p4))/4