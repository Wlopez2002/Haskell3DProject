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

data Rendr = SPRT Sprite | DSQR DSquare
instance Renderable Rendr where
    draw (SPRT x) = draw x
    draw (DSQR x) = draw x

instance Renderable DPoint where
    draw (DPoint x y z) (Player (DPoint px 0 pz) rl ud) = scale 1 1 $ translate px 0 $ circleSolid 2
    draw (DPoint x y z) (Player (DPoint px py pz) rl ud) = scale py py $ translate px 0 $ circleSolid 2
instance Show DPoint where
    show (DPoint x y z) = "(" ++ show x ++ "," ++ show y ++ "," ++ show z ++ ")"

data Sprite = Sprite {p :: DPoint, picts :: [Picture]}
instance Renderable Sprite where
    draw s (Player playerPoint rl ud) = 
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

data DSquare = DSquare {sp1 :: DPoint,sp2 :: DPoint,sp3 :: DPoint,sp4 :: DPoint, c :: Color}
instance Renderable DSquare where
    draw square (Player playerPoint rl ud) = 
        -- if every point is behind the player do not draw the square.
        if (isBehind rp1 && isBehind rp2 && isBehind rp3 && isBehind rp4)
            then
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
            -- Case where z is negative, mult instead of
            -- div, and use abs z.
            then (400*(x*(abs z)),300*(y*(abs z)))
            -- not sure what to do then -1 < z < 1
            -- for now i just have it use z = 1
            else (400*(x),300*(y))
        else (400*(x/z),300*(y/z))

 -- checks if a Dpoint is behind the player. Use for relative DPoint
isBehind :: DPoint -> Bool
isBehind (DPoint x y z) = if (z <= 0) then True else False

sortDrawElements :: DPoint -> [Rendr] -> [Rendr]
sortDrawElements pl s = fst $ unzip $ sortBy (flip compare `on` snd) (map (h pl) s)
    where
        h :: DPoint -> Rendr -> (Rendr, Float)
        h pp (DSQR sqr) = (DSQR sqr, squareDistance pp sqr)
        h pp (SPRT (Sprite sp picts)) = (SPRT (Sprite sp picts), getDistance pp sp)

-- Gets the average distance of the points of a DSquare from some point
squareDistance :: DPoint -> DSquare -> Float
squareDistance dp (DSquare p1 p2 p3 p4 c) = ((getDistance dp p1)+(getDistance dp p2)+(getDistance dp p3)+(getDistance dp p4))/4