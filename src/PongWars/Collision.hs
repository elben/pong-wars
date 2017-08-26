{-# LANGUAGE MultiWayIf #-}

module PongWars.Collision where

import Debug.Trace

-- | Mass of an Object. When two objects collide, the projection vector is set
-- for the object with less mass.
type Mass = Int

data Object =
  -- Axis-aligned bounding box.
  AABB Mass
       (Float, Float) -- (x, y) position
       Float          -- Half-width
       Float          -- Half-height

data Report
  = NotCollided
  | Collided Float -- Projection vector angle of collision points _into_ the
                   -- collision.
             Float -- Projection vector length
  deriving (Eq, Show)

checkCollision :: Object -> Object -> Report
checkCollision (AABB m1 (x1, y1) hw1 hh1) (AABB m2 (x2, y2) hw2 hh2) =
  let xAxisCollided =
        if | x1 <= x2 && (x1 + hw1) >= (x2 - hw2) ->
             let a = if m1 < m2 then 0.5 else 0
             in Collided a ((x1 + hw1) - (x2 - hw2))
           -- ^ Object 1 is left of 2, and 1 collided with 2 from the left.
           -- Projection vector value `d` is the amount of collision. Direction
           -- is the direction that the non-movable object should be moved to
           -- get out of collision.
           | x1 >= x2 && (x1 - hw1) <= (x2 + hw2) ->
             let a = if m1 < m2 then 0 else 0.5
             in Collided a ((x2 + hw2) - (x1 - hw1))
           -- ^ Object 1 is right of 2, and 1 collided with 2 from the right.
           | otherwise -> NotCollided
      yAxisCollided =
        if | y1 <= y2 && (y1 + hh1) >= (y2 - hh2) ->
             let a = if m1 < m2 then 0.75 else 0.25
             in Collided a ((y1 + hh1) - (y2 - hh2))
           -- ^ 1 collided with 2 from the top.
           | y1 >= y2 && (y1 - hh1) <= (y2 + hh2) ->
             let a = if m1 < m2 then 0.25 else 0.75
             in Collided a ((y2 + hh2) - (y1 - hh1))
           -- ^ 1 collided with 2 from the bottom.
           | otherwise -> NotCollided
  in
    if allAxisCollided [xAxisCollided, yAxisCollided]
      then traceShowId $ findProjectionVector [xAxisCollided, yAxisCollided]
      else NotCollided

-- | True if all axis has collided, which means a collision happened.
allAxisCollided :: [Report] -> Bool
allAxisCollided = all isCollided

-- | Find the smallest Collided report, which will be the projection vector, the
-- minimum amount needed to move the object to make it not collide. If one is
-- NotCollided, always choose the other one.
findProjectionVector :: [Report] -> Report
findProjectionVector = foldl takeSmaller NotCollided

-- | Choose the smaller collision report, by its projection vector value. If one
-- is NotCollided, always choose the other one.
takeSmaller :: Report -> Report -> Report
takeSmaller a@(Collided _ l1) b@(Collided _ l2) =
  if l1 <= l2 then a else b
takeSmaller NotCollided r = r
takeSmaller r NotCollided = r

isCollided :: Report -> Bool
isCollided NotCollided = False
isCollided _ = True
