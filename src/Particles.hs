module Particles (Particle(Particle), (^)) where

import Data.Maybe
import Prelude hiding ((^))
import Control.Applicative ((<|>))
import Control.Arrow
import qualified Data.Text.IO as T

type Vec = (Float, Float)
data Particle = Particle {
  mass     :: Float,
  radius   :: Float,
  position :: Vec,
  velocity :: Vec
} deriving Show

type Box = (Vec, Vec)

type Time = Float
type TimeDelta = Float

instance (Num a, Num b) => Num (a, b) where
  (w, x) + (y, z) = (w + y, x + z)
  (w, x) * (y, z) = (w * y, x * z)
  (w, x) - (y, z) = (w - y, x - z)
  abs (x, y)      = (abs x, abs y)
  signum (x, y)   = (signum x, signum y)
  fromInteger x   = (fromInteger x, fromInteger x)

-- dot product
(^) :: Vec -> Vec -> Float
(w, x) ^ (y, z) = w*y + x*z

(<>) :: Vec -> Vec -> Vec
(w, x) <> (y, z) = (w + y, x + z)

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


stepParticle :: TimeDelta -> Particle -> Particle
stepParticle dt (Particle m r p v) = Particle m r (p + v*(dt, dt)) v

isOverlapping :: Particle -> Particle -> Bool
isOverlapping p1 p2 = distSquared <= radius p1 + radius p2 where
  distSquared = (position p1 - position p2) ^ (position p1 - position p2)


(-*) :: Float -> Vec -> Vec
f -* (a, b) = (f*a, f*b)

collide :: Particle -> Particle -> (Particle, Particle)
collide particle1 particle2 = (particle1', particle2') where
  (Particle m1 r1 pos1 vel1) = particle1
  (Particle m2 r2 pos2 vel2) = particle2
  particle1' = Particle m1 r1 pos1 vel1'
  particle2' = Particle m1 r1 pos1 vel2'
  (u1, u2) = (getcomp (-axis) vel1, getcomp axis vel2)
  v1 = ((m1 - m2)*u1 + (2*m2)*u2) / (m1 + m2)
  v2 = ((2*m1)*u1 + (m2 - m1)*u2) / (m1 + m2)
  vel1' = set_axis_comp vel1 v1
  vel2' = set_axis_comp vel2 v2
  axis :: Vec
  axis = normalize (pos2 - pos1)
  normalize vec =  (1.0 / sqrt (vec ^ vec)) -* vec
  getcomp :: Vec -> Vec -> Float
  getcomp vec axis = vec ^ axis
  set_axis_comp :: Vec -> Float -> Vec
  set_axis_comp vec val = (vec - axis_component) + (val-*axis) where
    axis_component = (vec ^ axis) -* axis

-- Given a paricle and a wall return Nothing if the particle will never collide
-- with the wall or Maybe (t, pnew) where t is the amoutn of time in the future
-- in which it will collide and pnew is the particle with it's new position and
-- trajectory
data Wall = Void
collideWall :: Particle -> Wall -> Maybe(Time, Particle)
collideWall = undefined

simulate :: TimeDelta -> Box -> [Particle] -> [(Time, [Particle])]
simulate = undefined

p1 = Particle 1 1 (0, 0) (0, 0)
p2 = Particle 1 1 (-1, -1) (1, 1)
main :: IO ()
main = print $ collide p1 p2
-- main  = print $ reverse [1, 2, 3, 4, 5]
