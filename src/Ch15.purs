module Ch15 where

import Prelude

import Data.Profunctor (class Profunctor, dimap)


data Moore s a b = Moore s (s -> b) (s -> a -> s)

instance mooreProfunctor :: Profunctor (Moore s) where
-- (a' → a) → (b → b') → p a b → p a' b'
  dimap f g (Moore s extract transition) = Moore s extract' transition' 
    where
      extract'    = extract >>> g
      transition' = \z a' -> transition z (f a')