module Ch17 where

import Data.Functor.App
import Prelude

import Data.Bifunctor (class Bifunctor)
import Data.Either (Either(..))
import Data.Newtype (class Newtype, wrap)

newtype Validation a b = Validation (Either a b)

derive instance validationNewtype :: Newtype (Validation a b) _
derive instance validationEq :: (Eq a, Eq b) => Eq (Validation a b)
derive instance validationOrd :: (Ord a, Ord b) => Ord (Validation a b)
derive newtype instance validationShow :: (Show a, Show b) => Show (Validation a b)
derive newtype instance validationFunctor :: Functor (Validation a)
derive newtype instance validationBiFunctor :: Bifunctor Validation
instance applyValidation :: Semigroup a => Apply (Validation a) where
  apply (Validation (Right f)) (Validation (Right x))   = Validation $ Right $ f x
  apply (Validation (Left er1)) (Validation (Left er2)) = Validation $ Left $ er1 <> er2
  apply (Validation (Left err)) _                       = Validation $ Left $ err
  apply _                       (Validation (Left err)) = Validation $ Left $ err
instance validationApplicative :: Semigroup a => Applicative (Validation a) where
  pure = Validation <<< Right

newtype Name = Name String
derive newtype instance nameShow :: Show Name
newtype Age  = Age Int
derive newtype instance ageShow :: Show Age
derive instance newtypeAge :: Newtype Age _
type FamilyAgesRow r = 
  ( fatherAge :: Age
  , motherAge :: Age
  , childAge :: Age 
  | r
  )
type FamilyNamesRow r = 
  ( fatherName :: Name
  , motherName :: Name
  , childName :: Name 
  | r
  )
  
newtype FamilyAges = FamilyAges { | FamilyAgesRow ()}
derive newtype instance familyAges :: Show FamilyAges
newtype Family = Family { | FamilyNamesRow (FamilyAgesRow ())}

type LowerAge = Age
type UpperAge = Age

validateAge :: LowerAge -> UpperAge -> Age -> String -> Validation (Array String) Age
validateAge (Age lower) (Age upper) (Age age) who 
  | age > upper = Validation $ Left $ [who <> " to old"]
  | age < lower = Validation $ Left $ [who <> " to young"]
  | otherwise   = Validation $ Right $ wrap age

createFamilyAges :: { | FamilyAgesRow ()} -> Validation (Array String) FamilyAges
createFamilyAges { fatherAge, motherAge, childAge } =
  make
  <$> (validateAge (Age 20) (Age 90) fatherAge "father")
  <*> (validateAge (Age 20) (Age 90) motherAge "mother")
  <*> (validateAge (Age 0) (Age 18) childAge "child")
  where
    make :: Age -> Age -> Age -> FamilyAges
    make = \f m c -> FamilyAges { fatherAge: f, motherAge: m, childAge: c }