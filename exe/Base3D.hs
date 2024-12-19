module Base3D where

{-
This file contains all the needed code that multiple other files will
need to share. For example DPoint is needed by both Physics3D and Graphics3D.
-}

-- TODO: DPoint and operations with DPoint should be replaced with code
--       From a linear algebra library. 
data DPoint = DPoint {x :: Float, y :: Float, z :: Float}

data Player = Player {
    location :: DPoint,
    rotationLR :: Float, -- radians
    rotationUD :: Float -- radians
}

-- Gets the distance between two DPoints
getDistance :: DPoint -> DPoint -> Float
getDistance (DPoint x1 y1 z1) (DPoint x2 y2 z2) =
    sqrt((x2-x1)*(x2-x1)+(y2-y1)*(y2-y1)+(z2-z1)*(z2-z1))

-- Gets the difference between two DPoints
getDiff :: DPoint -> DPoint -> DPoint 
getDiff (DPoint x2 y2 z2) (DPoint x1 y1 z1) =
    (DPoint (x2-x1) (y2-y1) (z2-z1))