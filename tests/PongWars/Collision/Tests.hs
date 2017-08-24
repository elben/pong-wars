module PongWars.Collision.Tests where

import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit ((@?=))

import PongWars.Collision

tests :: Test
tests = testGroup "PongWars.Collision.Tests" $ concat
    [
      testCheckCollision
    ]

testCheckCollision :: [Test]
testCheckCollision =
  [
    -- +-----+
    -- |     |
    -- |  1  |
    -- |     |  +---+
    -- +-----+  | 2 |
    --          +---+
    testCase "1 to the left of 2 with y-axis collision" $
    checkCollision (AABB (10, 10) 5 10) (AABB (17, 17) 5 5) @?= NotCollided

    -- Same as above, but flip 1 and 2 labels
  , testCase "1 to the right of 2 with y-axis collision" $
    checkCollision (AABB (17, 17) 5 5) (AABB (10, 10) 10 20)  @?= NotCollided

    -- +-----+
    -- |     |
    -- |  1  |
    -- |     |
    -- +-----+
    --       +---+
    --       | 2 |
    --       +---+
  , testCase "1 above 2 with x-axis collision" $
    checkCollision (AABB (10, 10) 5 10) (AABB (10, 50) 5 5) @?= NotCollided

  -- Same as above, but flip 1 and 2 labels
  , testCase "2 above 1 with x-axis collision" $
    checkCollision (AABB (10, 50) 5 5) (AABB (10, 10) 5 10) @?= NotCollided
  ]
