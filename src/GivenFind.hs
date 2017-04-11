{- |
   Module     : GivenFind
   License    : GNU GENERAL PUBLIC LICENSE
   Maintainer : Jagoda "juliagoda" Górska <juliagoda.de@gmail.com>
   Portability: portable

Written by Jagoda "juliagoda" Górska,  juliagoda.de@gmail.com

Please start with the introduction at "GivenFind#intro".
-}

module GivenFind
( Symbols(..) 
, WhereData(..)
, mapText
, removefirstPuncs
, removelastPuncs
, joinSndWords
, checkIfListEmpty
, textToString
, stringToText
, secureConvert
, secureConvertMonad
)
where
    
import Prelude
import Control.Monad
import qualified Data.Text as T
import qualified Data.Map as M
import Control.Applicative
import Data.Maybe

{- $intro
 #intro#
Welcome to GivenFind.

This module provides search functions over texts in two formats - String
or Text from Data.Text module. It is designed to get questions, commands
and data with symbols and units from exercise of different branches. Most of the functions
gets only one parameter - your exercise in String or Text format. If you think, that a list
of units and symbols are not sufficient, you can use functions that take two
parameters - your text and searched symbol or even list of symbols. 

 If there are duplicates of names, I suggest using:

>import qualified GivenFind.Physics as P
>import qualified GivenFind.Base as G

-}


data Symbols a = Symbol a| None deriving (Show, Eq, Ord, Read)

data WhereData = OneLeft | OneRight | TwoRight deriving (Eq, Ord, Show)

instance Functor Symbols where 
    fmap f (Symbol x) = Symbol (f x)
    fmap f None = None 
    
instance Applicative Symbols where  
    pure = Symbol
    None <*> _ = None
    (Symbol f) <*> something = fmap f something  

instance Monad Symbols  where
    return a = Symbol a
    Symbol x >>= y = y x
    None >>= y = None
    Symbol x >> Symbol y = Symbol y
    None >> Symbol y = None
    Symbol x >> None = None
    fail _ = None


-- splits text to list of words (splitting thanks to white spaces)                   
mapText :: String -> [String]
mapText text = removefirstPuncs . removelastPuncs . words $ text


arePuncs :: Char -> Bool
arePuncs x
    | x == '!' = True
    | x == '.' = True
    | x == ',' = True
    | x == '?' = True
    | x == '(' = True
    | x == ')' = True
    | otherwise = False


-- removes "!.,?" characters from end and begin of list elements of [Char]
removefirstPuncs :: [String] -> [String]
removefirstPuncs xs = map (\x -> if arePuncs (head x) then tail x else x) xs


removelastPuncs :: [String] -> [String]
removelastPuncs xs = map (\x -> if arePuncs (last x) then init x else x) xs

-- values from dictionary with the same keys, are bound to a list
joinSndWords :: (Ord k) => [(k, a)] -> M.Map k [a]  
joinSndWords xs = M.fromListWith (++) $ map (\(k,v) -> (k,[v])) xs 

checkIfListEmpty :: Maybe [a] -> Maybe [a]
checkIfListEmpty list
    | (length (fromJust list)) > 0 = list
    | otherwise = Nothing
    
-- converts Maybe [T.Text] to Maybe [String]
textToString :: Maybe [T.Text] -> Maybe [String]
textToString texts = texts >>= \b -> return $ map T.unpack b

-- converts Maybe [String] to Maybe [T.Text]
stringToText :: Maybe [String] -> Maybe [T.Text]
stringToText texts = texts >>= \b -> return $ map T.pack b

-- secure searching in text during conversions
secureConvert :: [String] -> Int -> String
secureConvert text indexNumb = if indexNumb < 0 || indexNumb > (length text - 1) then "" else text !! indexNumb

-- secure searching in text during conversions
secureConvertMonad :: Maybe [String] -> Int -> Maybe String
secureConvertMonad text indexNumb = if indexNumb < 0 || indexNumb > (((fromJust . fmap length)  text) - 1) then Just "" else liftM (!! indexNumb) text
