module Parser where

import Prelude

import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)
import Data.String.CodeUnits (uncons)
import Data.Traversable (class Traversable, sequence)
import Data.Tuple (Tuple(..))
import Data.Unfoldable (class Unfoldable, none, replicate)
import Effect (Effect)
import Effect.Class.Console (log)

type ParserState a = Tuple String a
class ParserError (e :: Type) where
  eof :: e
type ParserFunction e a = 
  ParserError e => 
  String ->
  Either e (ParserState a)
newtype Parser e a = Parser (ParserFunction e a)

instance parserFunctor :: Functor (Parser e) where
  map f (Parser pf) = Parser $ \str -> map f <$> pf str

instance parserApply :: Apply (Parser e) where
  -- apply :: ∀ e a b. Parser e (a -> b) -> Parser e a -> Parser e b
  apply (Parser fab) (Parser fa) = Parser $ \str -> case fab str of
    Left e            -> Left e
    Right (Tuple s f) -> case fa s of
       Left e              -> Left e
       Right (Tuple s2 a) -> Right $ Tuple s2 (f a)

instance parserApplicative :: Applicative (Parser e) where
  pure a = Parser $ \str -> Right (Tuple str a)


parse :: ∀ e a. ParserError e => Parser e a -> ParserFunction e a
parse (Parser f) = f

data PError 
  = EOF

derive instance perrorGeneric :: Generic PError _
instance showError :: Show PError where
  show = genericShow
instance perrorParserError :: ParserError PError where
  eof = EOF

char :: ∀ e. ParserError e => Parser e Char
char = Parser \s -> case uncons s of
  Nothing           -> Left eof
  Just {head, tail} -> Right $ Tuple tail head

count :: ∀ e a f.
  Traversable f =>
  Unfoldable f =>
  Int ->
  Parser e a ->
  Parser e (f a)
count n p 
  | n <= 0    = pure none
  | otherwise = sequence (replicate n p)
test :: Effect Unit
test = log "chuj"