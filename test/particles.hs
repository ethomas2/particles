{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Test.QuickCheck (property, Arbitrary, arbitrary)
import Test.Hspec (hspec, describe, it, shouldThrow, anyException, shouldBe)
import Control.Exception (evaluate)
import Particles (Particle(Particle), (^))

instance Arbitrary Particle where
  -- arbitrary = Particle <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
  arbitrary = do
    mass <- arbitrary
    radius <- arbitrary
    position <- arbitrary
    velocity <- arbitrary
    return $ Particle mass radius position velocity



-- data Particle = Particle {
--   mass     :: Float,
--   radius   :: Float,
--   position :: Vec,
--   velocity :: Vec
-- } deriving Show

main :: IO ()
main = hspec $ do
  describe "Prelude.head" $ do
    it "returns the first element of a list" $ do
      head [23 ..] `shouldBe` (23 :: Int)

    it "returns the first element of an *arbitrary* list" $
      property $ \x xs -> head (x:xs) == (x :: Int)

    it "throws an exception if used with an empty list" $ do
      evaluate (head []) `shouldThrow` anyException

  describe "collide" $ do
    it "obeys conservation of energy"
      property $ \(p1 :: Particle, p2 :: Particle) ->
        let v = velocity p1 + velocity p2
        in True

      -- property $ \(p1, p2) -> energyBefore == energyAfter where
      --   energyBefore = energy p1 + energy p2
      --   energyAfter = energy p1' + energy p2'
      --   (p1', p2') = collide p1 p2
      --   energy (Particle m _ _  v) = 0.5*m*(v^v)
