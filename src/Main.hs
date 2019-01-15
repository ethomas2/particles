module Main where

import Data.Maybe
import Prelude hiding ((^))
import Control.Applicative ((<|>))

type Point = (Float, Float)
data Particle = Particle {
  mass     :: Float,
  radius   :: Float,
  position :: Point,
  velocity :: Point
} deriving Show
type Time = Float
type TimeDelta = Float

-- dot product
(^) :: Point -> Point -> Float
(w, x) ^ (y, z) = w*y + x*z


lift2 :: (a->u) -> (b->v) -> (a,b) -> (u,v)
lift2 fa fb (a,b) = (fa a, fb b)

instance (Num a, Num b) => Num (a,b) where
  fromInteger n   = (fromInteger n, fromInteger n)
  (a,b) + (a',b') = (a+a',b+b')
  (a,b) - (a',b') = (a-a',b-b')
  (a,b) * (a',b') = (a*a',b*b')
  negate = lift2 negate negate
  abs    = lift2 abs abs
  signum = lift2 signum signum

require :: (a -> Bool) -> a -> Maybe a
require condition a
  | condition a = Just a
  | otherwise = Nothing

-- Given two particles return Nothing if they will never collide along their
-- current trajectories or Just (t) where t is the amount of time
-- in the future in which they will collide
timeToCollide :: Particle -> Particle -> Maybe Time
timeToCollide p0 p1 = soln1 <|> soln2  where
  (Particle _ r0 x0 v0) = p0
  (Particle _ r1 x1 v1) = p1
  discriminant = require (>= 0) (b*b - 4*a*c)
  soln1 :: Maybe Time
  soln1 = do
    dval <- discriminant
    require (>=0) ((-b + sqrt dval) / 2*a)
  soln2 :: Maybe Time
  soln2 = do
    dval <- discriminant
    require (>=0) ((-b - sqrt dval) / (2*a))
  a = v0^v0 - v1^v1 - 2*v0^v1 :: Float
  b = 2*x0^v0 + 2*x1^v1 - 2*x0^v1 - 2*x1^v0 :: Float
  c = x0^x0 + x1^x1 - 2*x0^x1 - (r0 + r1)*(r0 + r1) :: Float

-- TODO: rewrite with lenses? Is that overkill? Probably
step :: TimeDelta -> [Particle] -> [Particle]
step delta particles = stepParticle <$> particles where
  stepParticle :: Particle -> Particle
  stepParticle (Particle m r pos vel) =
      Particle m r (pos + (delta, delta)*vel) vel

isColliding :: Particle -> Particle -> Bool
isColliding p1 p2 = distanceSquared <= radius p1 + radius p2 where
  distanceSquared = (position p1 - position p2) ^ (position p1 - position p2)

-- Given two particles return Nothing if they will never collide along their
-- current trajectories or Just (t, p1new, p2new) where t is the amount of time
-- in the future in which they will collide and p1new, p2new are the new
-- particles with their new positions/velocities
collide :: Particle -> Particle -> Maybe (Time, Particle, Particle)
collide = undefined

-- Given a paricle and a wall return Nothing if the particle will never collide
-- with the wall or Maybe (t, pnew) where t is the amoutn of time in the future
-- in which it will collide and pnew is the particle with it's new position and
-- trajectory
data Wall = Void
collideWall :: Particle -> Wall -> Maybe(Time, Particle)
collideWall = undefined

simulate :: [Particle] -> [(Time, [Particle])]
simulate = undefined


test_p1 :: Particle
test_p1 = Particle 1 1 (0, 0)  (0, 0)
test_p2 :: Particle
test_p2 = Particle 1 1 (5, 0) (-1, 0)
main :: IO ()
main = print $ timeToCollide test_p1 test_p2
