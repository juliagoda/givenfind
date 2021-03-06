{- |
   Module     : GivenFind
   License    : GNU GENERAL PUBLIC LICENSE
   Maintainer : Jagoda "juliagoda" Górska <juliagoda.de@gmail.com>
   Portability: portable

Written by Jagoda "juliagoda" Górska,  juliagoda.de@gmail.com

Please start with the introduction at "GivenFind#intro".
-}


{- $intro
 #intro#
Welcome to GivenFind.

This module provides search functions over texts in two formats - String
or Text from Data.Text module. It is designed to get questions, commands
and data with symbols and units from exercise of different branches. Most of the functions
gets only one parameter - your exercise in String or Text format. If you think, that a list
of units and symbols are not sufficient, you can use functions that take two
parameters - your text and searched symbol or even list of symbols. 

 If there are duplicates of names, I suggest using for example:

>import qualified GivenFind.Physics as P
>import qualified GivenFind.Geography as G
>import qualified GivenFind.Chemistry as Ch
...

-}


module GivenFind
( Symbols(..) 
, mapText
, removefirstPuncs
, removelastPuncs
, joinSndWords
, checkIfListEmpty
, textToString
, stringToText
, secureConvert
, secureConvertMonad
, getUnitsNumber
)
where
    
import Prelude
import Control.Monad
import qualified Data.Text as T
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.List
import Data.Char


data Symbols a = Symbol a| None deriving (Show, Eq, Ord, Read)

-- data WhereData = OneLeft | OneRight | TwoRight deriving (Eq, Ord, Show)

instance Functor Symbols where 
    fmap f (Symbol x) = Symbol (f x)
    fmap _ None = None 
    
instance Applicative Symbols where  
    pure = Symbol
    None <*> _ = None
    (Symbol f) <*> something = fmap f something  

instance Monad Symbols  where
    return a = Symbol a
    Symbol x >>= y = y x
    None >>= _ = None
    Symbol _ >> Symbol y = Symbol y
    None >> Symbol _ = None
    Symbol _ >> None = None
    None >> None = None
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

-- finds the easiest form of units - number space unit
-- returns all words, that have at the end units of length
getUnitsNumber :: [String] -> String -> Maybe [String] -> Maybe [String]
getUnitsNumber wordList prevEl (Just (x:xs)) =  case help wordList x of
                                             True -> if (isDigit . head) x then liftM (x :) (getUnitsNumber wordList x (Just xs)) else if (isDigit . head) prevEl then liftM ((prevEl ++ x) :) (getUnitsNumber wordList x $ Just xs) else getUnitsNumber wordList x $ Just xs
                                             False -> getUnitsNumber wordList x $ Just xs

getUnitsNumber _ _  (Just _) = Just []
getUnitsNumber _ _ _ = Nothing



-- if given word has end, that matches to any word from given list, returns True. For example if "cm" isInfixOf "123cm"
help :: [String] -> String -> Bool
help (x:xs) word = case isInfixOf x word of
                        True -> True
                        _ -> help xs word
help _ _ = False
