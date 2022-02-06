module Main
  ( main,
    apply
  )
  where

import Ch7 as Ch7
import Prelude hiding (apply)

import Effect (Effect)

main :: Effect Unit 
main = do
  let _ = flip const 1 2
  Ch7.test

apply :: ∀ a b. (a -> b) -> a -> b
apply = identity 