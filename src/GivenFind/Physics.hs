{-# LANGUAGE MultiParamTypeClasses
            ,FlexibleInstances
            ,TypeSynonymInstances #-}
            
module GivenFind.Physics  
( SearchPhySymbols(..)  
) where 

import Prelude hiding (lookup)
import qualified Data.Map.Strict as M
import GivenFind
import GivenFind.Questions
import Control.Monad
import Control.Applicative
import qualified Text.Read as R
import qualified Data.Text as T
import qualified Data.List as L
import Data.Maybe

    
    
class SearchInText s => SearchPhySymbols s where
    
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
    
    
instance SearchPhySymbols String where
    
    searchYourOneSymb text symbol =  liftM (head) $ convertFiltred (mapText text) $ takeOnlyTrue (mapText text) $ bindList $ check (singleton symbol) $ listSub (mapText text) (singleton symbol) 
    
    searchYourAllSymb text symbols = convertFiltred (mapText text) $ takeOnlyTrue (mapText text) $ bindList $ check symbols $ listSub (mapText text) symbols
    
    searchOnePhysSymb text = liftM (head) $ convertFiltred (mapText text) $ takeOnlyTrue (mapText text) $ bindList $ check (searchFirstPassed (mapText text) phySymbols) $ listSub (mapText text) (searchFirstPassed (mapText text) phySymbols) 
    
    searchAllPhysSymb text = convertFiltred (mapText text) $ takeOnlyTrue (mapText text) $ bindList $ check phySymbols $ listSub (mapText text) phySymbols
    
    searchAllPhys text = convertFiltredMore (mapText text) $ takeOnlyTrue (mapText text) $ bindList $ check phySymbols $ listSub (mapText text) phySymbols
    
    
instance SearchPhySymbols T.Text where
    
    searchYourOneSymb text symbol = liftM (head) $ convertFiltredToText $ convertFiltred (mapText (T.unpack text)) $ takeOnlyTrue (mapText (T.unpack text)) $ bindList $ check (singleton (T.unpack symbol)) $ listSub (mapText (T.unpack text)) (singleton (T.unpack symbol)) 
    
    searchYourAllSymb text symbols = convertFiltredToText $ convertFiltred (mapText (T.unpack text)) $ takeOnlyTrue (mapText (T.unpack text)) $ bindList $ check (map T.unpack symbols) $ listSub (mapText (T.unpack text)) (map T.unpack symbols)
    
    searchOnePhysSymb text = liftM (head) $ convertFiltredToText $ convertFiltred (mapText (T.unpack text)) $ takeOnlyTrue (mapText (T.unpack text)) $ bindList $ check phySymbols $ listSub (mapText (T.unpack text)) (searchFirstPassed (mapText (T.unpack text)) phySymbols) 
    
    searchAllPhysSymb text = convertFiltredToText $ convertFiltred (mapText (T.unpack text)) $ takeOnlyTrue (mapText (T.unpack text)) $ bindList $ check phySymbols $ listSub (mapText (T.unpack text)) phySymbols
    
    searchAllPhys text = convertFiltredToTextMore $ convertFiltredMore (mapText (T.unpack text)) $ takeOnlyTrue (mapText (T.unpack text)) $ bindList $ check phySymbols $ listSub (mapText (T.unpack text)) phySymbols
    
    
    
singleton :: a -> [a]
singleton a = [a]


-- if in a list of String have been found some element, then element is returned and function is stopped
searchFirstPassed :: [String] -> [String] -> [String]
searchFirstPassed text (x:xs) = if elem x text then [x] else searchFirstPassed text xs
searchFirstPassed _ [] = []


-- instead of empty list, it gives [-1] as not found value or found index of element in text
changeElemInd :: Eq a => a -> [a] -> [Int]
changeElemInd el text = case L.elemIndices el text of 
               [] -> [-1]
               _ -> L.elemIndices el text

               
-- gets through list of words to find indices and puts indexes numbers in places of words in list
listSub :: [String] -> [String] -> [[Int]]
listSub _ [[]] = [[-1]]
listSub text (x:xs) = (changeElemInd x text) : (listSub text xs) -- [Char]
listSub _ _ = init [[-1]] 



-- concatenates list of symbols with found indices
zipSymbolandIndex :: [String] -> [[Int]] -> [(String, [Int])]
zipSymbolandIndex list1 list2 = zip list1 list2


-- dictionary, where keys are units and values are symbols
symbWords :: [(String, String)]
symbWords = ([("m","s"), ("meters","s"), ("meter","s"), ("kg","m"), ("s","t"), ("seconds","t"), ("second","t"), ("m/s2","a"), ("m/s^2","a"), ("m/s","v"), ("mi/hr","v"), ("kg * m/s","p"), ("J","W"), ("W","P"), ("N","Fc"), ("N","Fw"), ("kg/m3","ρ"), ("kg/m^3","ρ"), ("Pa","p"), ("m","λ"), ("Hz","f"), ("K","T"), ("D","Z"), ("Ω","R"), ("V","U"), ("A","I"), ("J","E"), ("J / kg oC","Cwater"), ("1N * m2/kg2","G"), ("1N * m^2/kg^2","G"), ("1N * m^2 / kg^2","G"), ("1N * m^2/kg^2","G")])


-- list of units
phySymbols :: [String]
phySymbols = ["m","meters","meter","kg","s","seconds","second","m/s2","m/s^2","m/s","kg * m/s","J","W","N","kg/m3","kg/m^3","Pa","Hz","K","D","Ω","V","A","J / kg oC", "J/kgC", "J / kg * C", "J/kg * C","1N * m2/kg2", "1N * m^2/kg^2", "1N * m^2 / kg^2", "1N * m^2/kg^2"]


-- same units : m, J and N have different symbols
chooseFromText :: String -> [String] -> String
chooseFromText unit text = case unit of
                                "J" -> if elem "work" text || elem "work" text then fromJust $ liftM (!! 1) $ M.lookup "J" $ joinSndWords symbWords else fromJust $ liftM (!! 0) $ M.lookup "J" $ joinSndWords symbWords
                                "m" -> if elem "Wavelength" text || elem "wavelength" text then fromJust $ liftM (!! 0) $ M.lookup "m" $ joinSndWords symbWords else fromJust $ liftM (!! 1) $ M.lookup "m" $ joinSndWords symbWords
                                "N" -> if elem "buoyancy" text || elem "Buoyancy" text then fromJust $ liftM (!! 0) $ M.lookup "N" $ joinSndWords symbWords else fromJust $ liftM (!! 1) $ M.lookup "N" $ joinSndWords symbWords
                                _ -> fromJust $ liftM (!! 0) $ M.lookup unit $ joinSndWords symbWords

                                
-- example
-- ll = zip phySymbols (listSub ["magnetic","cubic","area","m^3","m^3"] phySymbols)

-- removes non found elements with -1
check :: [String] -> [[Int]] -> [(String,  [Int])]
check list1 list2 = filter (not . null . filter (/=(-1)) . snd) $ zipSymbolandIndex list1 list2


-- converts from Symbols WhereData to real position of number
--findIndexData :: Int -> Symbols WhereData -> Int 
--findIndexData actIndex element = let
--                                        indeks (Symbol OneLeft) = actIndex - 1
--                                        indeks None = -5
--                                    in indeks $ element

                                    
-- converts position of unit number to Symbols WhereData
--findSymbolData :: [String] -> Int -> Symbols WhereData
--findSymbolData text actIndex = let
--                                        indeks (Symbol (-1)) = Symbol OneLeft
--                                        indeks None = None
--                                    in indeks $ convertNumbInt text actIndex



-- zipped symbols with every matched index in two-dimensional list
bindList ::  [(String,  [Int])] -> [[(String, Int)]]
bindList (x:xs) = bindInts (fst x) (snd x) : bindList xs
bindList [] = []


bindInts :: String -> [Int] -> [(String, Int)]
bindInts el (x:xs) = (el, x)  : bindInts el xs
bindInts _ [] = [] 


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
