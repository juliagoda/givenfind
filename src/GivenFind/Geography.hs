{-# LANGUAGE MultiParamTypeClasses
            ,FlexibleInstances
            ,TypeSynonymInstances
            ,OverlappingInstances
            ,TemplateHaskell #-}
             
module GivenFind.Geography  
( listOfHours
, listOfScales
, listOfNominalScales
, listOfDistances
, listOfLevels
, listOfTemperatures
, listOfTitudes
, listOfDegrees
, Titudes(..)
) where  

import GivenFind.Base
import Control.Monad
import Control.Applicative
import Data.Geolocation.Reverse
import Geo.Computations
import Data.Geolocation.Reverse.Types as T
import Data.Maybe
import Data.Char
import Text.Read
import Prelude hiding (lookup)
import qualified Data.Map as M
import Data.List
import Data.List.Split
import Data.Angle
import Data.Time.LocalTime
import Data.Fixed
import qualified Data.Text as T


listOfHours :: String -> Maybe [TimeOfDay]
listOfHours txt = checkIfListEmpty . convertHours . getHours . return . removelastPuncs . removefirstPuncs $ mapText txt

listOfScales :: String -> Maybe [String]
listOfScales txt = checkIfListEmpty . checkForwNumbers . return . removelastPuncs . removefirstPuncs $ mapText txt

listOfNominalScales :: String -> Maybe [String]
listOfNominalScales txt = let
                              getText = return . removelastPuncs . removefirstPuncs $ mapText txt
                              in
                                  checkIfListEmpty . itScales (getNominalIndexes getText) getText
             
listOfDistances :: String -> Maybe [String]
listOfDistances txt = checkIfListEmpty . getDistances distanceSymb (convShortTypes ((return . removelastPuncs . removefirstPuncs . mapText) txt) "") ""
                                  
listOfLevels :: String -> Maybe [String]
listOfLevels txt = checkIfListEmpty . getLevels "" . convertLvlTypes . return . removelastPuncs . removefirstPuncs $ mapText txt

listOfTemperatures :: String -> Maybe [String]
listOfTemperatures txt = checkIfListEmpty . getTemperatures "" . return . removelastPuncs . removefirstPuncs $ mapText txt

listOfTitudes :: String -> Maybe [Titudes]
listOfTitudes txt = checkIfListEmpty . convertTitudes . getTitudes . return . removelastPuncs . removefirstPuncs $ mapText txt

listOfDegrees :: String -> Maybe [Degrees Double]
listOfDegrees txt = checkIfListEmpty . convertDegrees . reduceDegChar . getDegrees . return . removelastPuncs . removefirstPuncs $ mapText txt

--newtype Latitude = Latitude Double deriving (Eq, Show, Ord) -- N / S

--newtype Longitude = Longitude Double deriving (Eq, Show, Ord)  -- W / E


distanceSymb :: [String]
distanceSymb =  ["km","kilometers","m","meters","cm","centimeters","mm","milimeters","kilometer","centimeter","milimeter"]

surfaceSymb :: [String]
surfaceSymb = ["cm2","m2","km2","mm2","cm^2","m^2","km^2","mm^2"]

shortNumb :: [(String, String)]
shortNumb = [("tys.","000"),("tys","000"),("mln.","000000"),("mln","000000"),("mld.","000000000"),("mld","000000000")]

tempNumb :: [String]
tempNumb = ["oC","^oC","°C","°F","oF","^oF","°K","oK","^oK","°N","oN","^oN"]

latiLonList :: [String]
latiLonList = ["°N","°S","°W","°E","`N","`S","`W","`E","oN","oS","oW","oE","^oN","^oS","^oW","^oE"]


checkIfListEmpty :: Maybe [a] -> Maybe [a]
checkIfListEmpty list
    | (length (fromJust list)) > 0 = list
    | otherwise = Nothing

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

-- values from dictionary with the same keys, are bound to a list
joinSndWords :: (Ord k) => [(k, a)] -> M.Map k [a]  
joinSndWords xs = M.fromListWith (++) $ map (\(k,v) -> (k,[v])) xs 

ifHasScale :: String -> Maybe Bool
ifHasScale el = case any (==':') el of 
                         True -> if ((=='1') . head) el && ((==':') . (!! 1)) el then return True else return False
                         _ -> return $ False
                         

checkForwNumbers :: Maybe [String] -> Maybe [String]
checkForwNumbers (Just (x:xs)) = case ifHasScale x of 
                                             Just True -> liftM2 (:) (Just (x ++ (concat . takeWhile (\s -> ((readMaybe s :: Maybe Int) /= Nothing))) xs)) (checkForwNumbers (Just xs))
                                             _ -> checkForwNumbers (Just xs)
    
checkForwNumbers (Just _) = Just []
checkForwNumbers _ = Nothing


getNominalIndexes :: Maybe [String] -> [Int]
getNominalIndexes list = fromJust $ list >>= \b -> return $ findIndices (=="->") b

getNominalScales :: Maybe [String] -> Int -> Maybe String
getNominalScales list indexes = let
                                    backward = liftM (!! (indexes-1)) list
                                    forward = liftM (!! (indexes+1)) list
                                    optForward = liftM (!! (indexes+2)) list
                                    optBool = help distanceSymb optForward
                                in 
                                    if optBool then Just (++) `ap` (Just (++) `ap` (Just (++) `ap` (Just (++) `ap` (liftM (!! (indexes-2)) list) `ap` backward) `ap` (Just "->")) `ap` forward) `ap` optForward else (Just (++) `ap` (Just (++) `ap` backward `ap` (Just "->")) `ap` forward)

                                    
help :: [String] -> Maybe String -> Bool
help (x:xs) word = case liftM (isInfixOf x) word of
                        Just True -> True
                        _ -> help xs word
help _ word = False


itScales :: [Int] -> Maybe [String] -> Maybe [String]
itScales (x:xs) list = liftM2 (:) (getNominalScales list x) (itScales xs list)
itScales _ list = Just []



getDistances :: [String] -> Maybe [String] -> String -> Maybe [String]
getDistances wordList  (Just (x:xs)) prevEl =  case help wordList (Just x) of
                                             True -> if (isDigit . head) x then liftM (x :) (getDistances wordList (Just xs) x) else liftM ((prevEl ++ x) :) (getDistances wordList (Just xs) x)
                                             False -> getDistances wordList (Just xs) x
                                             
getDistances wordList  (Just _) prevEl = Just []
getDistances wordList _ prevEl = Nothing


convShortTypes :: Maybe [String] -> String -> Maybe [String]
convShortTypes (Just (x:xs)) prevEl
    | any (==x) (map fst shortNumb) = liftM2 (:) (liftM (prevEl ++) (liftM concat (M.lookup x (joinSndWords shortNumb)))) (convShortTypes (Just xs) x)
    | otherwise = liftM (x :) (convShortTypes (Just xs) x)
    
convShortTypes (Just _) prevEl = Just []
convertShortTypes _ prevEl = Nothing


convertLvlTypes :: Maybe [String] -> Maybe [String]
convertLvlTypes (Just (x:y:z:xs))
    | (x == "above" || x == "below") = if (y == "sea" && z == "level") then liftM ((intercalate " " [x,y,z]) :) (convertLvlTypes (Just xs)) else liftM (x :) (convertLvlTypes (liftM (y :) ((liftM (z :) (Just xs))))) 
    | otherwise = liftM (x :) (convertLvlTypes (liftM (y :) ((liftM (z :) (Just xs)))))
    
convertLvlTypes (Just (x:xs)) = liftM (x :) (convertLvlTypes (Just xs))
    
convertLvlTypes (Just _) = Just []
convertLvlTypes _ = Nothing


getLevels :: String -> Maybe [String] -> Maybe [String]
getLevels prevEl (Just (x:y:xs)) =  case help distanceSymb (Just x) && (isInfixOf "sea level" y) of
                                             True -> if (isDigit . head) x then liftM ((intercalate " " [x,y]) :) (getLevels x (Just xs)) else liftM ((intercalate " " [prevEl ++ x,y]) :) (getLevels x (Just xs))
                                             False -> getLevels x (liftM (y :)(Just xs))
                                             
getLevels prevEl (Just _) = Just []
getLevels prevEl _ = Nothing


getTemperatures :: String -> Maybe [String] -> Maybe [String]
getTemperatures prevEl (Just (x:xs)) = case help tempNumb (Just x) of
                                             True -> if ((=='+') . head) x || ((=='-') . head) x || (isDigit . head) x  then liftM (x :) (getTemperatures x (Just xs)) else liftM ((prevEl ++ x) :) (getTemperatures x (Just xs))
                                             False -> getTemperatures x (Just xs)
                                             
getTemperatures prevEl (Just _) = Just []
getTemperatures prevEl _ = Nothing


getHours :: Maybe [String] -> Maybe [String]
getHours (Just (x:y:z:xs)) = case x == "at" of
                                             True -> case any (==':') y && (length y == 5 || length y == 4)  of
                                                          True -> liftM ((y ++ (takeAmPm z)) :) (getHours (liftM (y :) (liftM (z :) (Just xs))))
                                                          _ -> getHours (liftM (y :) (liftM (z :) (Just xs)))
                                             False -> getHours (liftM (y :) (liftM (z :) (Just xs)))
                                             
getHours (Just (_:xs)) = getHours $ Just xs
                                     
getHours (Just _) = Just []                               
getHours _ = Nothing

convertHours :: Maybe [String] -> Maybe [TimeOfDay]
convertHours hoursList = hoursList >>= mapM (\x -> return (if (find (==' ') x) /= Nothing then partsOfTime (splitOn ":" (takeWhile (/=' ') x)) else partsOfTime (splitOn ":" x)))
                                                                                         
partsOfTime :: [String] -> TimeOfDay
partsOfTime timeList
    | length timeList == 2 = TimeOfDay (read (head timeList) :: Int)  (read (last timeList) :: Int) (0 :: Pico)
    | length timeList == 3 = TimeOfDay (read (head timeList) :: Int)  (read (timeList !! 1) :: Int) (read (last timeList) :: Pico)
    | otherwise = midnight


takeAmPm :: String -> String
takeAmPm el
    | el == "a.m." = " a.m."
    | el == "p.m." = " p.m."
    | el == "AM" = " a.m"
    | el == "PM" = " p.m"
    | otherwise = ""
    
    
getTitudes :: Maybe [String] -> Maybe [String]
getTitudes (Just (x:xs)) =  case help latiLonList (Just x) of
                                             True -> case (isDigit . head) x of
                                                          True -> liftM (x :) (getTitudes (Just xs))
                                                          _ -> getTitudes (Just xs)
                                             _ -> getTitudes (Just xs)
                                             
getTitudes (Just _) = Just []
getTitudes _ = Nothing


data Titudes = Width T.Latitude | Height T.Longitude | None deriving (Show, Read, Eq)

convertTitudes :: Maybe [String] -> Maybe [Titudes]
convertTitudes titudesList = titudesList >>= mapM (\x -> return (helpFunc (splitOneOf "o`," x)))

helpFunc :: [String] -> Titudes
helpFunc tab
    | last tab == "N" = if length tab < 3 then Width (T.Latitude (readMaybe (head tab) :: Maybe Double)) else Width $ T.Latitude (readMaybe (intercalate "." [head tab, tab !! 1]) :: Maybe Double)
    | last tab == "S" = if length tab < 3 then Width (T.Latitude (readMaybe (show(-(read (head tab) :: Double))) :: Maybe Double)) else Width $ T.Latitude (readMaybe (show(-(read (intercalate "." [head tab, tab !! 1]) :: Double))) :: Maybe Double)
    | last tab == "E" = if length tab < 3 then Height (T.Longitude (readMaybe (head tab) :: Maybe Double)) else Height $  T.Longitude (readMaybe (intercalate "." [head tab, tab !! 1]) :: Maybe Double)
    | last tab == "W" = if length tab < 3 then Height (T.Longitude (readMaybe (show(-(read (head tab) :: Double))) :: Maybe Double)) else Height $ T.Longitude (readMaybe (show(-(read (intercalate "." [head tab, tab !! 1]) :: Double))) :: Maybe Double)

    
getDegrees :: Maybe [String] -> Maybe [String]
getDegrees (Just (x:xs)) =  case (isDigit . head) x && ((isInfixOf "°" x) || (isInfixOf "`" x) || (isInfixOf "o" x)) of
                                             True -> case (isInfixOf "`" x) of
                                                          True -> liftM ((map (\s -> if s == 'o' then '.' else s) x) :) (getDegrees (Just xs))
                                                          _ -> liftM (x :) $ getDegrees $ Just xs
                                             _ -> getDegrees $ Just xs
                                             
getDegrees (Just _) = Just []
getDegrees _ = Nothing

reduceDegChar :: Maybe [String] -> Maybe [String]
reduceDegChar list = list >>= \b -> return $ map (filter(\x -> (isDigit x) || (isPunctuation x))) b

convertDegrees :: Maybe [String] -> Maybe [Degrees Double]
convertDegrees degreesList = degreesList >>= mapM (\x -> return (Degrees (read x :: Double)))
