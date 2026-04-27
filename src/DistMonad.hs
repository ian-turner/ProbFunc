{-# LANGUAGE InstanceSigs #-}
module DistMonad where


-- | Probability monad definition
type Probability = Float
data Dist a = D {unD :: [(a, Probability)]}
              deriving (Show, Eq)

instance Monad Dist where
  return x    = D [(x,1)]
  (D d) >>= f = D [(y,p*q) | (x,p) <- d,
                             (y,q) <- unD (f x)]

instance Functor Dist where
  fmap :: (a -> b) -> Dist a -> Dist b
  fmap f (D d) = D [(f x, p) | (x,p) <- d]

instance Applicative Dist where
  pure x = D [(x,1)]
  (D fd) <*> (D xd) = D [(f x, p*q) | (x,p) <- xd, (f,q) <- fd]


-- | Utility functions
uniform :: [a] -> Dist a
uniform xs = let n = fromIntegral (length xs) in D [(x,1/n) | x <- xs]
