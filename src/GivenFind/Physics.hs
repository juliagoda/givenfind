{-# LANGUAGE MultiParamTypeClasses
            ,FlexibleInstances
            ,TypeSynonymInstances
            ,OverlappingInstances #-}
            
module GivenFind.Physics  
( SearchSymbols(..)  
, Symbols(..)
) where 

import Prelude hiding (lookup)
import qualified Data.Map as M
import GivenFind.Base
import Control.Monad
import Control.Applicative
import qualified Text.Read as R
import qualified Data.Text as T
import qualified Data.List as L
import Data.Maybe



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
    
    
class SearchInText s => SearchSymbols s where
    
    -- finds given unit and number in a text, if doesn't find - returns None
    searchYourOneSymb :: s -> s -> Symbols (s, Double)
    
    -- finds given units list and their numbers in a text, if doesn't find - returns None
    searchYourAllSymb :: s -> [s] -> Symbols [(s, Double)]
    
    -- finds first unit from the phySymbols list and his numbers in a text, if doesn't find - returns None
    searchOnePhysSymb :: s -> Symbols (s, Double)
    
    -- finds all units from the phySymbols list and their numbers in a text, if doesn't find - returns None
    searchAllPhysSymb :: s -> Symbols [(s, Double)]
    
    -- finds all units from the phySymbols list and their numbers in a text. Additionally adds to list symbols, that are seen in physics formulas
    searchAllPhys :: s -> Symbols [(s, s)]
    
    
instance SearchSymbols String where
    
    searchYourOneSymb text symbol =  liftM (head) $ convertFiltred (mapText text) $ takeOnlyTrue (mapText text) $ bindList $ check (singleton symbol) $ listSub (mapText text) (singleton symbol) 
    
    searchYourAllSymb text symbols = convertFiltred (mapText text) $ takeOnlyTrue (mapText text) $ bindList $ check symbols $ listSub (mapText text) symbols
    
    searchOnePhysSymb text = liftM (head) $ convertFiltred (mapText text) $ takeOnlyTrue (mapText text) $ bindList $ check (searchFirstPassed (mapText text) phySymbols) $ listSub (mapText text) (searchFirstPassed (mapText text) phySymbols) 
    
    searchAllPhysSymb text = convertFiltred (mapText text) $ takeOnlyTrue (mapText text) $ bindList $ check phySymbols $ listSub (mapText text) phySymbols
    
    searchAllPhys text = convertFiltredMore (mapText text) $ takeOnlyTrue (mapText text) $ bindList $ check phySymbols $ listSub (mapText text) phySymbols
    
    
instance SearchSymbols T.Text where
    
    searchYourOneSymb text symbol = liftM (head) $ convertFiltredToText $ convertFiltred (mapText (T.unpack text)) $ takeOnlyTrue (mapText (T.unpack text)) $ bindList $ check (singleton (T.unpack symbol)) $ listSub (mapText (T.unpack text)) (singleton (T.unpack symbol)) 
    
    searchYourAllSymb text symbols = convertFiltredToText $ convertFiltred (mapText (T.unpack text)) $ takeOnlyTrue (mapText (T.unpack text)) $ bindList $ check (map T.unpack symbols) $ listSub (mapText (T.unpack text)) (map T.unpack symbols)
    
    searchOnePhysSymb text = liftM (head) $ convertFiltredToText $ convertFiltred (mapText (T.unpack text)) $ takeOnlyTrue (mapText (T.unpack text)) $ bindList $ check phySymbols $ listSub (mapText (T.unpack text)) (searchFirstPassed (mapText (T.unpack text)) phySymbols) 
    
    searchAllPhysSymb text = convertFiltredToText $ convertFiltred (mapText (T.unpack text)) $ takeOnlyTrue (mapText (T.unpack text)) $ bindList $ check phySymbols $ listSub (mapText (T.unpack text)) phySymbols
    
    searchAllPhys text = convertFiltredToTextMore $ convertFiltredMore (mapText (T.unpack text)) $ takeOnlyTrue (mapText (T.unpack text)) $ bindList $ check phySymbols $ listSub (mapText (T.unpack text)) phySymbols
    
    
    
singleton :: a -> [a]
singleton a = case a of a -> [a]
         
         
-- splits text to list of words (splitting thanks to white spaces)                   
mapText :: String -> [String]
mapText text = removefirstPuncs . removelastPuncs . words $ text


arePuncs :: Char -> Bool
arePuncs x
    | x == '!' = True
    | x == '.' = True
    | x == ',' = True
    | x == '?' = True
    | otherwise = False


-- removes "!.,?" characters from end and begin of list elements of [Char]
removefirstPuncs :: [String] -> [String]
removefirstPuncs xs = map (\x -> if arePuncs (head x) then tail x else x) xs


removelastPuncs :: [String] -> [String]
removelastPuncs xs = map (\x -> if arePuncs (last x) then init x else x) xs


-- if in a list of String have been found some element, then element is returned and function is stopped
searchFirstPassed :: [String] -> [String] -> [String]
searchFirstPassed text (x:xs) = if elem x text then [x] else searchFirstPassed text xs
searchFirstPassed text [] = []


-- instead of empty list, it gives [-1] as not found value or found index of element in text
changeElemInd :: Eq a => a -> [a] -> [Int]
changeElemInd el text = case L.elemIndices el text of 
               [] -> [-1]
               _ -> L.elemIndices el text

               
-- gets through list of words to find indices and puts indexes numbers in places of words in list
listSub :: [String] -> [String] -> [[Int]]
listSub text [[]] = [[-1]]
listSub text (x:xs) = (changeElemInd x text) : (listSub text xs) -- [Char]
listSub text _ = init [[-1]] -- trzeba to wyeliminowac


-- gets through list of words, where list of words is transformed to the next level of list [[[Char]]]
listSub2 :: [String] -> [[String]] -> [[[Int]]]
listSub2 text [[]] = [[[-1]]]
listSub2 text (x:xs) =  (listSub text x) : (listSub2 text xs) -- [[Char]]
listSub2 text _ = init [[[-1]]]


-- splits list of String into one two-dimensional list 
numbAngList :: [String] -> [[String]]
numbAngList list = map words list


-- concatenates list of symbols with found indices
zipSymbolandIndex :: [String] -> [[Int]] -> [(String, [Int])]
zipSymbolandIndex list1 list2 = zip list1 list2


-- dictionary, where keys are units and values are symbols
symbWords :: [(String, String)]
symbWords = ([("m","s"), ("meters","s"), ("meter","s"), ("kg","m"), ("s","t"), ("seconds","t"), ("second","t"), ("m/s2","a"), ("m/s^2","a"), ("m/s","v"), ("mi/hr","v"), ("kg * m/s","p"), ("J","W"), ("W","P"), ("N","Fc"), ("N","Fw"), ("kg/m3","ρ"), ("kg/m^3","ρ"), ("Pa","p"), ("m","λ"), ("Hz","f"), ("K","T"), ("D","Z"), ("Ω","R"), ("V","U"), ("A","I"), ("J","E"), ("J / kg oC","Cwater"), ("1N * m2/kg2","G"), ("1N * m^2/kg^2","G"), ("1N * m^2 / kg^2","G"), ("1N * m^2/kg^2","G")])


-- list of units
phySymbols :: [String]
phySymbols = ["m","meters","meter","kg","s","seconds","second","m/s2","m/s^2","m/s","kg * m/s","J","W","N","kg/m3","kg/m^3","Pa","Hz","K","D","Ω","V","A","J / kg oC", "J/kgC", "J / kg * C", "J/kg * C","1N * m2/kg2", "1N * m^2/kg^2", "1N * m^2 / kg^2", "1N * m^2/kg^2"]


-- values from dictionary with the same keys, are bound to a list
joinSndWords :: (Ord k) => [(k, a)] -> M.Map k [a]  
joinSndWords xs = M.fromListWith (++) $ map (\(k,v) -> (k,[v])) xs 


-- same units : m, J and N have different symbols
chooseFromText :: String -> [String] -> String
chooseFromText unit text = case unit of
                                "J" -> if elem "work" text || elem "work" text then fromJust $ liftM (!! 1) $ M.lookup "J" $ joinSndWords symbWords else fromJust $ liftM (!! 0) $ M.lookup "J" $ joinSndWords symbWords
                                "m" -> if elem "Wavelength" text || elem "wavelength" text then fromJust $ liftM (!! 0) $ M.lookup "m" $ joinSndWords symbWords else fromJust $ liftM (!! 1) $ M.lookup "m" $ joinSndWords symbWords
                                "N" -> if elem "buoyancy" text || elem "Buoyancy" text then fromJust $ liftM (!! 0) $ M.lookup "N" $ joinSndWords symbWords else fromJust $ liftM (!! 1) $ M.lookup "N" $ joinSndWords symbWords
                                _ -> fromJust $ liftM (!! 0) $ M.lookup unit $ joinSndWords symbWords

                                
-- example
ll = zip phySymbols (listSub ["magnetic","cubic","area","m^3","m^3"] phySymbols)

-- removes non found elements with -1
check :: [String] -> [[Int]] -> [(String,  [Int])]
check list1 list2 = filter (not . null . filter (/=(-1)) . snd) $ zipSymbolandIndex list1 list2


-- converts from Symbols WhereData to real position of number
findIndexData :: Int -> Symbols WhereData -> Int 
findIndexData actIndex element = let
                                        indeks (Symbol OneLeft) = actIndex - 1
                                        indeks None = -5
                                    in indeks $ element

                                    
-- converts position of unit number to Symbols WhereData
findSymbolData :: [String] -> Int -> Symbols WhereData
findSymbolData text actIndex = let
                                        indeks (Symbol (-1)) = Symbol OneLeft
                                        indeks None = None
                                    in indeks $ convertNumbInt text actIndex
                  
                  
-- converts Int to Symbols Int. Function is used for findSymbolData                                   
convertNumbInt :: [String] -> Int -> Symbols Int
convertNumbInt text curIndex
    | (R.readMaybe (secureConvert text (curIndex - 1)) :: Maybe Double) /= Nothing = Symbol (-1)
    | otherwise = None

    
-- secure searching in text during conversions
secureConvert :: [String] -> Int -> String
secureConvert text indexNumb = if indexNumb < 0 || indexNumb > (length text - 1) then "" else text !! indexNumb


-- zipped symbols with every matched index in two-dimensional list
bindList ::  [(String,  [Int])] -> [[(String, Int)]]
bindList (x:xs) = bindInts (fst x) (snd x) : bindList xs
bindList [] = []


bindInts :: String -> [Int] -> [(String, Int)]
bindInts el (x:xs) = (el, x)  : bindInts el xs
bindInts el [] = [] 


-- checks if number is one left from symbol, one right or two right steps. In places of [Int] are put [Symbols OneLeft] or [Symbols OneRight] or [Symbols TwoRight] or [None]
takeOnlyTrue :: [String] -> [[(String, Int)]] -> [(String, Int)]
takeOnlyTrue textSplitted bindedList = filter (ifConvertedNumbBool textSplitted . snd) $ concat bindedList


-- returns true, if at given pocision is number, otherwise returns false
ifConvertedNumbBool :: [String] -> Int -> Bool
ifConvertedNumbBool text curIndex
    | (R.readMaybe (secureConvert text (curIndex - 1)) :: Maybe Double) /= Nothing = True
    | otherwise = False

    
 -- zipped units with every matched index are transformed to Symbols [(unit,number of unit)]   
convertFiltred :: [String] -> [(String, Int)] -> Symbols [(String, Double)]
convertFiltred text filtredList 
    | length filtredList > 0 = return $ map (\p -> (fst p, convertNumbDouble text (snd p))) filtredList
    | otherwise = None
    
    
 -- zipped units with every matched index are transformed to Symbols [(symbol, number + unit)]   
convertFiltredMore :: [String] -> [(String, Int)] -> Symbols [(String, String)]
convertFiltredMore text filtredList 
    | length filtredList > 0 = return $ map (\p -> (chooseFromText (fst p) text, show (convertNumbDouble text (snd p)) ++ " " ++ (fst p))) filtredList
    | otherwise = None

    
-- Symbols [(unit,number of unit)] have the same values but other data type    
convertFiltredToText :: Symbols [(String, Double)] -> Symbols [(T.Text, Double)]
convertFiltredToText filtredList = do
    list <- filtredList
    return $ getZipList $ (,) <$> ZipList (map T.pack (map fst list)) <*> ZipList (map snd list)
  
  
 -- Symbols [(symbol, number + unit)] have the same values but other data types   
convertFiltredToTextMore :: Symbols [(String, String)] -> Symbols [(T.Text, T.Text)]
convertFiltredToTextMore filtredList = do
    list <- filtredList
    return $ getZipList $ (,) <$> ZipList (map T.pack (map fst list)) <*> ZipList (map T.pack (map snd list))

    
-- found number at given pocision is converted to double    
convertNumbDouble :: [String] -> Int -> Double
convertNumbDouble text curIndex
    | (R.readMaybe (secureConvert text (curIndex - 1)) :: Maybe Double) /= Nothing = read (text !! (curIndex - 1)) :: Double
    | otherwise = 0
