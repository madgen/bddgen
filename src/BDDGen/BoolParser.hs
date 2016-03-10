module BDDGen.BoolParser (parseBool) where

import Prelude hiding (exp)
import Text.Parsec
import Text.Parsec.Char

import BDDGen.Definition (BoolExp(..))

parseBool :: String -> Either ParseError BoolExp
parseBool = parse expression "<command line>"

expression = do
  e <- expHigh
  spaces
  eof
  return e

expHigh :: Parsec String () BoolExp
expHigh = try (do
    e <- exp
    spaces
    string "<->"
    h <- expHigh
    return $ Iff e h)
  <|> exp

exp :: Parsec String () BoolExp
exp = spaces *> do
    f <- factor
    e' <- exp' 
    case e' of
      [ ] -> return f
      fs -> return $ toImps (f:fs)

toImps :: [ BoolExp ] -> BoolExp
toImps [ ] = error "should not happen"
toImps [ x ] = x
toImps (x:y:rest) = toImps (Imply x y:rest)

exp' :: Parsec String () [BoolExp]
exp' = spaces *> (try (do
    string "->"
    f <- factor
    fs <- exp'
    return $ f:fs)
  <|> return [ ])

factor :: Parsec String () BoolExp
factor = spaces *> (try (do
    t <- term
    spaces
    char '|'
    f <- factor
    return $ Or t f)
  <|> term)

term :: Parsec String () BoolExp
term = spaces *> (try (do
    k <- key
    spaces
    char '&'
    t <- term
    return $ And k t)
  <|> (char '!' >> Neg <$> key)
  <|> key)

key :: Parsec String () BoolExp
key = spaces *> (Literal <$> many1 letter)
  <|> do 
    char '(' 
    e <- expHigh
    spaces
    char ')'
    return e
