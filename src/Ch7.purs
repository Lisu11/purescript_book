module Ch7 where

import Data.Eq (class Eq)
import Data.Generic.Rep (class Generic)
import Data.Ord (class Ord)
import Data.Show.Generic (genericShow)
import Effect (Effect)
import Effect.Console (log)
import Prelude (class Show, Ordering(..), Unit, compare, discard, show, ($), (<>), (==), (>))

data Maybe a
    = Nothing
    | Just a

instance eqMaybe :: Eq a => Eq (Maybe a) where
    eq Nothing Nothing   = true
    eq (Just x) (Just y) = x == y
    eq _ _               = false

-- derive instance ordMaybe :: Ord a => Ord (Maybe a)
instance ordMaybe :: Ord a => Ord (Maybe a) where
  compare Nothing Nothing   = EQ
  compare (Just x) (Just y) = compare x y
  compare Nothing _         = LT
  compare (Just _) _        = GT

greaterThanOrEq :: ∀ a. Ord a => a -> a -> Boolean
greaterThanOrEq x y = case compare x y of
  GT -> true
  EQ -> true
  _  -> false

infix 4 greaterThanOrEq as >=

-- instance showMaybe :: Show a => Show (Maybe a) where
--   show Nothing  = "Nothing"
--   show (Just x) = "Just " <> show x

derive instance genericMaybe :: Generic (Maybe a) _
instance showMaybe :: Show a => Show (Maybe a) where
  show = genericShow

test :: Effect Unit
test = do
  log $ show $ Just 5 == Just 5
  log $ show $ Just 5 > Nothing
  log $ show $ Just 5 >= Nothing
  log $ show $ Just "abc"