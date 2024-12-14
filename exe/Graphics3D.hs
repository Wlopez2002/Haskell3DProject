module Graphics3D where

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

data DPoint = DPoint {x :: Float, y :: Float, z :: Float}
instance Renderable DPoint where
    draw (DPoint x y z) (Player (DPoint px 0 pz) rl ud) = scale 1 1 $ translate px 0 $ circleSolid 2
    draw (DPoint x y z) (Player (DPoint px py pz) rl ud) = scale py py $ translate px 0 $ circleSolid 2
instance Show DPoint where
    show (DPoint x y z) = "(" ++ show x ++ "," ++ show y ++ "," ++ show z ++ ")"

data Player = Player {
    location :: DPoint,
    rotationLR :: Float, -- radians
    rotationUD :: Float -- radians
}


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