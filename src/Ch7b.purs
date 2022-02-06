module Ch7b where

import Data.Newtype
import Prelude

import Data.Generic.Rep (class Generic, Sum(..))
import Data.Int (fromString)
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)
import Data.String (Pattern(..), split)
import Data.String.Read (class Read, read)
import Effect (Effect)
import Effect.Console (log)

newtype CSV = CSV String
derive instance newtypeCSV :: Newtype CSV _

class ToCSV a where
  toCSV :: a -> CSV

newtype FullName = FullName String
derive instance newtypeFullName :: Newtype FullName _

newtype Age = Age Int
derive instance newtypeAge :: Newtype Age _

data Occupation
  = Doctor
  | Dentist
  | Lawer
  | Unemployed
derive instance genericOccupation :: Generic Occupation _
instance showOccupation :: Show Occupation where
  show = genericShow
instance readOccupation :: Read Occupation where
  read "Doctor"     = Just Doctor
  read "Dentist"    = Just Dentist
  read "Lawer"      = Just Lawer
  read "Unemployed" = Just Unemployed
  read _            = Nothing
  
newtype Person = Person
  { name :: FullName
  , age :: Age
  , occupation :: Occupation
  }
derive instance newtypePerson :: Newtype Person _

instance pertsonToCSV :: ToCSV Person where
  toCSV (Person p) = CSV $ name <> "," <> age <> "," <> occupation
                    where
                      name       = unwrap p.name
                      age        = show $ unwrap p.age
                      occupation = show p.occupation

class FromCSV a where
  fromCSV :: CSV -> Maybe a

instance fromCSVPerson :: FromCSV Person where
  fromCSV (CSV csv) = case split (Pattern ",") csv of
    [name, age, occupation] -> do
      age'        <- fromString age
      occupation' <- read occupation :: (Maybe Occupation)
      
      Just $ Person {
        name: FullName name,
        age: Age age',
        occupation: occupation'
      }
    _ -> Nothing

person :: Person
person = Person {
  name:       FullName "imie", 
  age:        Age 10,
  occupation: Lawer
}


test :: Effect Unit
test = do
  log $ unwrap $ toCSV person