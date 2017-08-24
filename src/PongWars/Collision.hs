{-# LANGUAGE MultiWayIf #-}

module PongWars.Collision where

data Object =
  -- Axis-aligned bounding box.
  AABB (Float, Float) -- (x, y) position
       Float          -- Half-width
       Float          -- Half-height

data Report
  = NotCollided
  | Collided Float -- Projection vector angle of collision
             Float -- Projection vector length
  deriving (Eq, Show)

checkCollision :: Object -> Object -> Report
checkCollision (AABB (x1, y1) hw1 hh1) (AABB (x2, y2) hw2 hh2) =
  let xAxisCollided =
        if | (x1 + hw1) >= (x2 - hw2) -> Collided 0.5 ((x1 + hw1) - (x2 - hw2))
           -- ^ 1 collided with 2 from the left. Projection angle points left.
           -- Projection vector value is the amount of collision.
           | (x1 - hw1) >= (x2 + hw2) -> Collided 0.0 ((x1 - hw1) - (x2 + hw2))
           -- ^ 1 collided with 2 from the right. Projection angle points right.
           | otherwise -> NotCollided
      yAxisCollided =
        if | (y1 + hh1) >= (y2 - hh2) -> Collided 0.25 ((y1 + hh1) - (y2 - hh2))
           -- ^ 1 collided with 2 from the bottom. Projection angle points up.
           | (y1 - hw1) >= (y2 + hh2) -> Collided 0.75 ((y1 - hw1) - (y2 + hh2))
           -- ^ 1 collided with 2 from the top. Projection angle points down.
           | otherwise -> NotCollided
  in
    if allAxisCollided [xAxisCollided, yAxisCollided]
      then findProjectionVector [xAxisCollided, yAxisCollided]
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
