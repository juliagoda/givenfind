{-# LANGUAGE MultiParamTypeClasses
            ,FlexibleInstances
            ,TypeSynonymInstances
            ,TemplateHaskell #-}
             
module GivenFind.Geography  
( listOfHours
, listOfScales
, listOfNominalScales
, listOfDistances
, listOfLevels
, listOfTemperatures
, listOfTitudes
, listOfRadians
) where  

import GivenFind
import GivenFind.Geography.Types
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
import Data.Time.LocalTime
import Data.Fixed


listOfHours :: String -> Maybe [TimeOfDay]
listOfHours txt = checkIfListEmpty . convertHours . getHours . return . removelastPuncs . removefirstPuncs $ mapText txt

listOfScales :: String -> Maybe [String]
listOfScales txt = checkIfListEmpty . checkForwNumbers . return . removelastPuncs . removefirstPuncs $ mapText txt

listOfNominalScales :: String -> Maybe [String]
listOfNominalScales txt = let
                              getText = return . removelastPuncs . removefirstPuncs $ mapText txt
                              in
                                  checkIfListEmpty $ itScales (getNominalIndexes getText) getText

listOfDistances :: String -> Maybe [Distance]
listOfDistances txt = checkIfListEmpty . convertDistances . getUnitsNumber distanceSymb "" . convShortTypes "" . return . removelastPuncs . removefirstPuncs . mapText $ txt

listOfLevels :: String -> Maybe [String]
listOfLevels txt = checkIfListEmpty . getLevels "" . convertLvlTypes . return . removelastPuncs . removefirstPuncs $ mapText txt

listOfTemperatures :: String -> Maybe [String]
listOfTemperatures txt = checkIfListEmpty . getTemperatures "" . return . removelastPuncs . removefirstPuncs $ mapText txt

listOfTitudes :: String -> Maybe [Titudes]
listOfTitudes txt = checkIfListEmpty . convertTitudes . getTitudes . return . removelastPuncs . removefirstPuncs $ mapText txt

listOfRadians :: String -> Maybe [Heading]
listOfRadians txt = checkIfListEmpty . convertDegrees . reduceDegChar . getDegrees . return . removelastPuncs . removefirstPuncs $ mapText txt


--newtype Latitude = Latitude Double deriving (Eq, Show, Ord) -- N / S

--newtype Longitude = Longitude Double deriving (Eq, Show, Ord)  -- W / E


distanceSymb :: [String]
distanceSymb =  ["km","kilometres","m","metres","cm","centimetres","mm","millimetres","kilometre","centimetre","millimetre"]

surfaceSymb :: [String]
surfaceSymb = ["cm2","m2","km2","mm2","cm^2","m^2","km^2","mm^2"]

shortNumb :: [(String, String)]
shortNumb = [("thous.","000"),("thou.","000"),("MM","000000"),("M","000000"),("G","000000000"),("B","000000000"),("BN","000000000")]

partOfMeters :: [(String, Double)]
partOfMeters = [("km",1000.0),("kilometres",1000.0),("kilometre",1000.0),("cm",0.01),("centimetre",0.01),("centimetres",0.01),("mm",0.001),("millimetres",0.001),("millimetre",0.001),("m",1.0),("metre",1.0),("metres",1.0)]

tempNumb :: [String]
tempNumb = ["oC","^oC","°C","°F","oF","^oF","°K","oK","^oK","°N","oN","^oN"]

latiLonList :: [String]
latiLonList = ["°N","°S","°W","°E","`N","`S","`W","`E","′N","′S","′W","′E","oN","oS","oW","oE","^oN","^oS","^oW","^oE"]


-- If at word's beginning are found characters "1:", it's a scale
ifHasScale :: String -> Bool
ifHasScale el = case any (==':') el of 
                         True -> if ((=='1') . head) el && ((==':') . (!! 1)) el then True else False
                         _ -> False


-- if "1:" are found at the word's beginning, takes from this point all numeric values and whitespaces (so it can take for example 1:100 000 000 and 1:100000000)
checkForwNumbers :: Maybe [String] -> Maybe [String]
checkForwNumbers (Just (x:xs)) = case ifHasScale x of 
                                            True -> liftM2 (:) (Just (x ++ (concat . takeWhile (\s -> ((readMaybe s :: Maybe Int) /= Nothing))) xs)) (checkForwNumbers $ Just xs)
                                            _ -> checkForwNumbers $ Just xs
    
checkForwNumbers (Just _) = Just []
checkForwNumbers _ = Nothing


-- finds all indices of "->" character
getNominalIndexes :: Maybe [String] -> [Int]
getNominalIndexes list = fromJust $ list >>= \b -> return $ findIndices (=="->") b


-- takes informations around string "->" to max two steps after and before string
getNominalScales :: Maybe [String] -> Int -> Maybe String
getNominalScales list indexes = let
                                    backward = secureConvertMonad list (indexes - 1)
                                    forward = secureConvertMonad list (indexes + 1)
                                    optForward = secureConvertMonad list (indexes + 2)
                                    optBool = help distanceSymb $ fromJust optForward
                                in 
                                    if optBool then Just (++) `ap` (Just (++) `ap` (Just (++) `ap` (Just (++) `ap` (secureConvertMonad list (indexes - 2)) `ap` backward) `ap` (Just "->")) `ap` forward) `ap` optForward else (Just (++) `ap` (Just (++) `ap` backward `ap` (Just "->")) `ap` forward)


-- if given word has end, that matches to any word from given list, returns True. For example if "cm" isInfixOf "123cm"
help :: [String] -> String -> Bool
help (x:xs) word = case isInfixOf x word of
                        True -> True
                        _ -> help xs word
help _ word = False


-- takes as argument indices of "->" and text in monad form and returns list of nominal scales
itScales :: [Int] -> Maybe [String] -> Maybe [String]
itScales (x:xs) list = liftM2 (:) (getNominalScales list x) (itScales xs list)
itScales _ list = Just []


-- converts units of length, that are different from meters (Distance is a type for distances in meters)
convertDistances :: Maybe [String] -> Maybe [Distance]
convertDistances distList = distList >>= mapM (\x -> return (partsOfDistance (takeWhile isDigit x) (dropWhile isDigit x)))


-- converts units of length, using from dictionary "partOfMeters"
partsOfDistance :: String -> String -> Distance
partsOfDistance distNumb distSymb = case liftM (!! 0) (M.lookup distSymb (joinSndWords partOfMeters)) of
                                         Nothing -> read distNumb :: Double
                                         Just x -> (read distNumb :: Double) * x


-- converts abbreviations to numeric values (for example thous. = 000) and prepends to beginning of value
convShortTypes :: String -> Maybe [String] -> Maybe [String]
convShortTypes prevEl (Just (x:xs))
    | any (==x) (map fst shortNumb) = liftM2 (:) (liftM (prevEl ++) ((liftM concat . M.lookup x . joinSndWords) shortNumb)) (convShortTypes x $ Just xs)
    | otherwise = liftM (x :) (convShortTypes x $ Just xs)
    
convShortTypes prevEl (Just _) = Just []
convertShortTypes prevEl _ = Nothing


-- looks for words "above" or "below", then "sea" and "level". Joins these words and adds to list
convertLvlTypes :: Maybe [String] -> Maybe [String]
convertLvlTypes (Just (x:y:z:xs))
    | (x == "above" || x == "below") = if (y == "sea" && z == "level") then liftM ((intercalate " " [x,y,z]) :) (convertLvlTypes (Just xs)) else liftM (x :) (convertLvlTypes (liftM (y :) (liftM (z :) $ Just xs))) 
    | otherwise = liftM (x :) (convertLvlTypes (liftM (y :) (liftM (z :) $ Just xs)))
    
convertLvlTypes (Just (x:xs)) = liftM (x :) (convertLvlTypes $ Just xs)
    
convertLvlTypes (Just _) = Just []
convertLvlTypes _ = Nothing


-- returns sea level rises with numeric values. Function is used after convertLvlTypes
getLevels :: String -> Maybe [String] -> Maybe [String]
getLevels prevEl (Just (x:y:xs)) =  case (help distanceSymb x) && (isInfixOf "sea level" y) of
                                             True -> if (isDigit . head) x then liftM ((intercalate " " [x,y]) :) (getLevels x (Just xs)) else liftM ((intercalate " " [prevEl ++ x,y]) :) (getLevels x $ Just xs)
                                             False -> getLevels x (liftM (y :) $ Just xs)
                                             
getLevels prevEl (Just _) = Just []
getLevels prevEl _ = Nothing


-- returns list of temperatures, if there were found values with '+', '-' or numeric value as first characters of word. Words should have matched endings from list "tempNumb"
getTemperatures :: String -> Maybe [String] -> Maybe [String]
getTemperatures prevEl (Just (x:xs)) = case help tempNumb x of
                                             True -> if ((=='+') . head) x || ((=='-') . head) x || (isDigit . head) x  then liftM (x :) (getTemperatures x (Just xs)) else liftM ((prevEl ++ x) :) (getTemperatures x $ Just xs)
                                             False -> getTemperatures x $ Just xs
                                             
getTemperatures prevEl (Just _) = Just []
getTemperatures prevEl _ = Nothing


-- finds and returns hours, minutes and seconds, if before time value is found the word "at". "AM" or "PM" can be but not have to after time values.
getHours :: Maybe [String] -> Maybe [String]
getHours (Just (x:y:z:xs)) = case x == "at" of
                                             True -> case any (==':') y && (length y == 5 || length y == 4)  of
                                                          True -> liftM ((y ++ (takeAmPm z)) :) (getHours (liftM (y :) (liftM (z :) $ Just xs)))
                                                          _ -> getHours (liftM (y :) (liftM (z :) $ Just xs))
                                             False -> getHours (liftM (y :) (liftM (z :) $ Just xs))
                                             
getHours (Just (_:xs)) = getHours $ Just xs
                                     
getHours (Just _) = Just []                               
getHours _ = Nothing


-- converts type for time values to TimeOfDay
convertHours :: Maybe [String] -> Maybe [TimeOfDay]
convertHours hoursList = hoursList >>= mapM (\x -> return (if (find (==' ') x) /= Nothing then partsOfTime (splitOn ":" (takeWhile (/=' ') x)) else partsOfTime (splitOn ":" x)))
 
 
-- if splitted text with character ":" has two elements, it contains hour:minutes, if three - hour:minutes:seconds
partsOfTime :: [String] -> TimeOfDay
partsOfTime timeList
    | length timeList == 2 = TimeOfDay (read (head timeList) :: Int)  (read (last timeList) :: Int) (0 :: Pico)
    | length timeList == 3 = TimeOfDay (read (head timeList) :: Int)  (read (timeList !! 1) :: Int) (read (last timeList) :: Pico)
    | otherwise = midnight


-- various detected descriptions are transformed to one form
takeAmPm :: String -> String
takeAmPm el
    | el == "a.m." = " a.m."
    | el == "p.m." = " p.m."
    | el == "AM" = " a.m"
    | el == "PM" = " p.m"
    | otherwise = ""
    

-- words with characteristic suffixes for longitudes and latitudes are added to list
getTitudes :: Maybe [String] -> Maybe [String]
getTitudes (Just (x:xs)) =  case help latiLonList x of
                                             True -> case (isDigit . head) x of
                                                          True -> liftM (x :) (getTitudes $ Just xs)
                                                          _ -> getTitudes $ Just xs
                                             _ -> getTitudes $ Just xs
                                             
getTitudes (Just _) = Just []
getTitudes _ = Nothing


-- found words from function "getTitudes" are converted from String to type Titudes, that contains Longitude as well as Latitude
convertTitudes :: Maybe [String] -> Maybe [Titudes]
convertTitudes titudesList = titudesList >>= mapM (\x -> (return . helpFunc . splitOneOf "o`,°′") x)


-- auxiliary function for converted values of found latitudes and longitudes
helpFunc :: [String] -> Titudes
helpFunc tab
    | last tab == "N" = if length tab < 3 then Width (T.Latitude (readMaybe (head tab) :: Maybe Double)) else Width $ T.Latitude (readMaybe (intercalate "." [head tab, tab !! 1]) :: Maybe Double)
    | last tab == "S" = if length tab < 3 then Width (T.Latitude (readMaybe (show(-(read (head tab) :: Double))) :: Maybe Double)) else Width $ T.Latitude (readMaybe (show(-(read (intercalate "." [head tab, tab !! 1]) :: Double))) :: Maybe Double)
    | last tab == "E" = if length tab < 3 then Height (T.Longitude (readMaybe (head tab) :: Maybe Double)) else Height $  T.Longitude (readMaybe (intercalate "." [head tab, tab !! 1]) :: Maybe Double)
    | last tab == "W" = if length tab < 3 then Height (T.Longitude (readMaybe (show(-(read (head tab) :: Double))) :: Maybe Double)) else Height $ T.Longitude (readMaybe (show(-(read (intercalate "." [head tab, tab !! 1]) :: Double))) :: Maybe Double)


-- finds and adds to list degrees with numeric values
getDegrees :: Maybe [String] -> Maybe [String]
getDegrees (Just (x:xs)) =  case (isDigit . head) x && ((isInfixOf "°" x) || (isInfixOf "`" x) || (isInfixOf "o" x)) of
                                             True -> case (isInfixOf "`" x) of
                                                          True -> liftM ((map (\s -> if s == 'o' then '.' else s) x) :) (getDegrees $ Just xs)
                                                          _ -> liftM (x :) $ getDegrees $ Just xs
                                             _ -> getDegrees $ Just xs
                                             
getDegrees (Just _) = Just []
getDegrees _ = Nothing


-- takes only numeric values from found degrees
reduceDegChar :: Maybe [String] -> Maybe [String]
reduceDegChar list = list >>= \b -> return $ map (filter(\x -> (isDigit x) || (isPunctuation x))) b


-- converts numeric values of found degrees to radians and type is changed to Heading (Heading is a type for angles expressed in radians)
convertDegrees :: Maybe [String] -> Maybe [Heading]
convertDegrees degreesList = degreesList >>= mapM (\x -> return (2 * pi * (read x :: Double) / 360))
