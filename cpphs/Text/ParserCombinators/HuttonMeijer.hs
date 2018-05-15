-----------------------------------------------------------------------------
-- |
-- Module      :  ParseLib
-- Copyright   :  ...
-- Copyright   :  Graham Hutton (University of Nottingham), Erik Meijer (University of Utrecht)
-- 
-- Maintainer  :  Malcolm Wallace <Malcolm.Wallace@cs.york.ac.uk>
-- Stability   :  Stable
-- Portability :  All
--
--                  A LIBRARY OF MONADIC PARSER COMBINATORS
-- 
--                               29th July 1996
-- 
--                  Graham Hutton               Erik Meijer
--             University of Nottingham    University of Utrecht
-- 
-- This Haskell script defines a library of parser combinators, and is
-- taken from sections 1-6 of our article "Monadic Parser Combinators".
-- Some changes to the library have been made in the move from Gofer
-- to Haskell:
-- 
--    * Do notation is used in place of monad comprehension notation;
-- 
--    * The parser datatype is defined using "newtype", to avoid the overhead
--      of tagging and untagging parsers with the P constructor.
-----------------------------------------------------------------------------


module Text.ParserCombinators.HuttonMeijer
   (Parser(..), item, first, papply, (+++), sat, {-tok,-} many, many1,
    sepby, sepby1, chainl,
    chainl1, chainr, chainr1, ops, bracket, char, digit, lower, upper,
    letter, alphanum, string, ident, nat, int, spaces, comment, junk,
    skip, token, natural, integer, symbol, identifier, reparse) where

import Data.Char
import Control.Monad

infixr 5 +++

type Token = Char

---------------------------------------------------------
-- | The parser monad

newtype Parser a   = P ([Token] -> [(a,[Token])])

instance Functor Parser where
   -- map         :: (a -> b) -> (Parser a -> Parser b)
   fmap f (P p)    = P (\inp -> [(f v, out) | (v,out) <- p inp])

instance Monad Parser where
   -- return      :: a -> Parser a
   return v        = P (\inp -> [(v,inp)])

   -- >>=         :: Parser a -> (a -> Parser b) -> Parser b
   (P p) >>= f     = P (\inp -> concat [papply (f v) out | (v,out) <- p inp])

   -- fail        :: String -> Parser a
   fail _          = P (\_ -> [])

instance MonadPlus Parser where
   -- mzero       :: Parser a
   mzero           = P (\_ -> [])

   -- mplus       :: Parser a -> Parser a -> Parser a
   (P p) `mplus` (P q)  = P (\inp -> (p inp ++ q inp))

-- ------------------------------------------------------------
-- * Other primitive parser combinators
-- ------------------------------------------------------------

item               :: Parser Token
item                = P (\inp -> case inp of
                                   []     -> []
                                   (x:xs) -> [(x,xs)])

first             :: Parser a -> Parser a
first (P p)        = P (\inp -> case p inp of
                                   []    -> []
                                   (x:_) -> [x])

papply            :: Parser a -> [Token] -> [(a,[Token])]
papply (P p) inp   = p inp

-- ------------------------------------------------------------
-- * Derived combinators
-- ------------------------------------------------------------

(+++)             :: Parser a -> Parser a -> Parser a
p +++ q            = first (p `mplus` q)

sat               :: (Token -> Bool) -> Parser Token
sat p              = do {x <- item; if p x then return x else mzero}

--tok               :: Token -> Parser Token
--tok t              = do {x <- item; if t==snd x then return t else mzero}

many              :: Parser a -> Parser [a]
many p             = many1 p +++ return []
--many p           = force (many1 p +++ return [])

many1             :: Parser a -> Parser [a]
many1 p            = do {x <- p; xs <- many p; return (x:xs)}

sepby             :: Parser a -> Parser b -> Parser [a]
p `sepby` sep      = (p `sepby1` sep) +++ return []

sepby1            :: Parser a -> Parser b -> Parser [a]
p `sepby1` sep     = do {x <- p; xs <- many (do {sep; p}); return (x:xs)}

chainl            :: Parser a -> Parser (a -> a -> a) -> a -> Parser a
chainl p op v      = (p `chainl1` op) +++ return v

chainl1           :: Parser a -> Parser (a -> a -> a) -> Parser a
p `chainl1` op     = do {x <- p; rest x}
                     where
                        rest x = do {f <- op; y <- p; rest (f x y)}
                                 +++ return x

chainr            :: Parser a -> Parser (a -> a -> a) -> a -> Parser a
chainr p op v      = (p `chainr1` op) +++ return v

chainr1           :: Parser a -> Parser (a -> a -> a) -> Parser a
p `chainr1` op     = do {x <- p; rest x}
                     where
                        rest x = do {f <- op; y <- p `chainr1` op; return (f x y)}
                                 +++ return x

ops               :: [(Parser a, b)] -> Parser b
ops xs             = foldr1 (+++) [do {p; return op} | (p,op) <- xs]

bracket           :: Parser a -> Parser b -> Parser c -> Parser b
bracket open p close = do {open; x <- p; close; return x}

-- ------------------------------------------------------------
-- * Useful parsers
-- ------------------------------------------------------------

char              :: Char -> Parser Char
char x             = sat (\y -> x == y)

digit             :: Parser Char
digit              = sat isDigit

lower             :: Parser Char
lower              = sat isLower

upper             :: Parser Char
upper              = sat isUpper

letter            :: Parser Char
letter             = sat isAlpha

alphanum          :: Parser Char
alphanum           = sat isAlphaNum +++ char '_'

string            :: String -> Parser String
string ""          = return ""
string (x:xs)      = do {char x; string xs; return (x:xs)}

ident             :: Parser String
ident              = do {x <- lower; xs <- many alphanum; return (x:xs)}

nat               :: Parser Int
nat                = do {x <- digit; return (fromEnum x - fromEnum '0')} `chainl1` return op
                     where
                        m `op` n = 10*m + n

int               :: Parser Int
int                = do {char '-'; n <- nat; return (-n)} +++ nat

-- ------------------------------------------------------------
-- * Lexical combinators
-- ------------------------------------------------------------

spaces            :: Parser ()
spaces             = do {many1 (sat isSpace); return ()}

comment           :: Parser ()
--comment            = do {string "--"; many (sat (\x -> x /= '\n')); return ()}
--comment            = do 
--                       _ <- string "--"
--                       _ <- many (sat (\x -> x /= '\n'))
--                       return ()
comment            = do
                       bracket (string "/*") (many item) (string "*/")
                       return ()

junk              :: Parser ()
junk               = do {many (spaces +++ comment); return ()}

skip              :: Parser a -> Parser a
skip p             = do {junk; p}

token             :: Parser a -> Parser a
token p            = do {v <- p; junk; return v}

-- ------------------------------------------------------------
-- * Token parsers
-- ------------------------------------------------------------

natural           :: Parser Int
natural            = token nat

integer           :: Parser Int
integer            = token int

symbol            :: String -> Parser String
symbol xs          = token (string xs)

identifier        :: [String] -> Parser String
identifier ks      = token (do {x <- ident;
                                if not (elem x ks) then return x
                                else return mzero})

--- Push some tokens back onto the input stream and reparse ------------------

-- | This is useful for recursively expanding macros.  When the
--   user-parser recognises a macro use, it can lookup the macro
--   expansion from the parse state, lex it, and then stuff the
--   lexed expansion back down into the parser.
reparse    :: [Token] -> Parser ()
reparse ts  = P (\inp-> [((), ts++inp)])

------------------------------------------------------------------------------

