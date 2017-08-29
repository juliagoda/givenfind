{-# LANGUAGE MultiParamTypeClasses
            ,FlexibleInstances
            ,TypeSynonymInstances
            ,TemplateHaskell #-}
             
module GivenFind.Chemistry  
( SearchChemSymbols(..)
) where  

import GivenFind
import GivenFind.Questions
import GivenFind.Geography (listOfTemperatures, listOfDistances)
import Control.Monad
import Control.Applicative
import Radium.Element
import Radium.Formats.Condensed
import Radium.Formats.Smiles
import Data.Maybe
import Data.Char
import Text.Read
import Prelude hiding (lookup)
import qualified Data.Text as DT
import qualified Data.Map.Strict as M
import Data.List
import Data.List.Split

-- density -> g/cm3, g/cm^3, g/mL, kg/l, kg/L, lbs/L, lbs/l
-- mass -> g, kg, mg, gram, grams, L, mL, Liters, liters, ga, oz, ounces
-- temperatures -> ["oC","^oC","°C","°F","oF","^oF","°K","oK","^oK","°N","oN","^oN"]
--  subatomic particles -> electron, electrons, proton, protons, neutron, neutrons
-- pseudo-units -> ppm, parts-per-million, ppb, parts-per-billion, ppt, parts-per-trillion, ppq, parts-per-quadrillion
-- table groups -> 1A, 2A, 3A, 4A, 5A, 6A, 7A, 8A, IA, IIA, IIIA, IVA, VA, VIA, VIIA, VIIIA
-- chemical reactions -> combination, decomposition, combustion, single replacement, double replacement
-- atomic mass -> g/mol
-- moles -> mol, moles
-- atoms -> atom, atoms
-- molecules -> molecule, molecules
-- pressure -> atm, torr
-- physical constants -> L-atm/mol-deg, gas constant, gas constants
-- oxidation numbers -> oxidation number, oxidation numbers



class SearchInText s => SearchChemSymbols s where
    
    -- finds for example "5 cm2"
    listOfSurfaces :: s -> Maybe [s]
    
    -- finds for example "4 lbs/l"
    listOfDens :: s -> Maybe [s]
    
    -- finds for example "5 kg"
    listOfMass :: s -> Maybe [s]
    
    -- finds for example "3 neutrons"
    listOfSubAtoms :: s -> Maybe [s]
    
    -- finds for example "6 ppt"
    listOfPseudoUnits :: s -> Maybe [s]
    
    -- finds for example "2 L-atm/mol-deg"
    listOfConstants :: s -> Maybe [s]
    
    -- finds for example "2 atm"
    listOfPress :: s -> Maybe [s]
    
    -- finds for example "group IIA"
    listOfGroups :: s -> Maybe [s]
    
    -- finds for example "period 1"
    listOfPeriods :: s -> Maybe [s]
    
    -- finds for example "7 moles"
    listOfMoles :: s -> Maybe [s]
    
    -- finds for example "10 atoms"
    listOfAtoms :: s -> Maybe [s]
    
    -- finds for example "6 molecules"
    listOfMolecules :: s -> Maybe [s]
    
    -- finds for example "1 oxidation number"
    listOfOxNumb :: s -> Maybe [s]
    
    -- finds for example "carbon" or "H"
    listOfElem :: s -> Maybe [Element]
    
    
    
    
instance SearchChemSymbols String where
    
    listOfSurfaces txt = checkIfListEmpty . getUnitsNumber surfaceSymb "" $ return . removelastPuncs . removefirstPuncs $ mapText txt
    
    listOfDens txt = checkIfListEmpty . getUnitsNumber densSymb "" $ return . removelastPuncs . removefirstPuncs $ mapText txt
    
    listOfMass txt = checkIfListEmpty . getUnitsNumber massSymb "" $ return . removelastPuncs . removefirstPuncs $ mapText txt
    
    listOfSubAtoms txt = checkIfListEmpty . getUnitsNumber subAtoms "" $ return . removelastPuncs . removefirstPuncs $ mapText txt
    
    listOfPseudoUnits txt = checkIfListEmpty . getUnitsNumber pseudoUnits "" $ return . removelastPuncs . removefirstPuncs $ mapText txt
    
    listOfConstants txt = checkIfListEmpty . getUnitsNumber physConstants "" $ return . removelastPuncs . removefirstPuncs $ mapText txt
    
    listOfPress txt = checkIfListEmpty . getUnitsNumber pressSymb "" $ return . removelastPuncs . removefirstPuncs $ mapText txt
    
    listOfGroups txt = checkIfListEmpty $ getFromTable ( (return . removelastPuncs . removefirstPuncs)  (mapText txt)) ["group","Group","groups","Groups","electron structure of the"]
    
    listOfPeriods txt = checkIfListEmpty $ getFromTable ( (return . removelastPuncs . removefirstPuncs)  (mapText txt)) ["period","Period","periods","Periods"]
    
    listOfMoles txt = checkIfListEmpty . getUnitsNumber ["mole","moles"] "" $ return . removelastPuncs . removefirstPuncs $ mapText txt
    
    listOfAtoms txt = checkIfListEmpty . getUnitsNumber ["atom","atoms"] "" $ return . removelastPuncs . removefirstPuncs $ mapText txt
    
    listOfMolecules txt = checkIfListEmpty . getUnitsNumber ["molecule","molecules"] "" $ return . removelastPuncs . removefirstPuncs $ mapText txt
    
    listOfOxNumb txt = checkIfListEmpty . getUnitsNumber ["oxidation number","oxidation numbers"] "" $ return . removelastPuncs . removefirstPuncs $ mapText txt
    
    listOfElem txt = checkIfListEmpty . getElements . appendChemResults chemSymb . removelastPuncs . removefirstPuncs $ mapText txt

    
    
instance SearchChemSymbols DT.Text where
    
    listOfSurfaces txt = checkIfListEmpty . liftM (map DT.pack) . getUnitsNumber surfaceSymb "" $ return . removelastPuncs . removefirstPuncs $ mapText $ DT.unpack txt
    
    listOfDens txt = checkIfListEmpty . liftM (map DT.pack) . getUnitsNumber densSymb "" $ return . removelastPuncs . removefirstPuncs $ mapText $ DT.unpack txt
    
    listOfMass txt = checkIfListEmpty . liftM (map DT.pack) . getUnitsNumber massSymb "" $ return . removelastPuncs . removefirstPuncs $ mapText $ DT.unpack txt
    
    listOfSubAtoms txt = checkIfListEmpty . liftM (map DT.pack) . getUnitsNumber subAtoms "" $ return . removelastPuncs . removefirstPuncs $ mapText $ DT.unpack txt
    
    listOfPseudoUnits txt = checkIfListEmpty . liftM (map DT.pack) . getUnitsNumber pseudoUnits "" $ return . removelastPuncs . removefirstPuncs $ mapText $ DT.unpack txt
    
    listOfConstants txt = checkIfListEmpty . liftM (map DT.pack) . getUnitsNumber physConstants "" $ return . removelastPuncs . removefirstPuncs $ mapText $ DT.unpack txt
    
    listOfPress txt = checkIfListEmpty . liftM (map DT.pack) . getUnitsNumber pressSymb "" $ return . removelastPuncs . removefirstPuncs $ mapText $ DT.unpack txt
    
    listOfGroups txt = checkIfListEmpty $ liftM (map DT.pack) $ getFromTable ( (return . removelastPuncs . removefirstPuncs)  (mapText . DT.unpack $ txt)) ["group","Group","groups","Groups","electron structure of the"]
    
    listOfPeriods txt = checkIfListEmpty $ liftM (map DT.pack) $ getFromTable ( (return . removelastPuncs . removefirstPuncs)  (mapText . DT.unpack $ txt)) ["period","Period","periods","Periods"]
    
    listOfMoles txt = checkIfListEmpty . liftM (map DT.pack) . getUnitsNumber ["mole","moles"] "" $ return . removelastPuncs . removefirstPuncs $ mapText $ DT.unpack txt
    
    listOfAtoms txt = checkIfListEmpty . liftM (map DT.pack) . getUnitsNumber ["atom","atoms"] "" $ return . removelastPuncs . removefirstPuncs $ mapText $ DT.unpack txt
    
    listOfMolecules txt = checkIfListEmpty . liftM (map DT.pack) . getUnitsNumber ["molecule","molecules"] "" $ return . removelastPuncs . removefirstPuncs $ mapText $ DT.unpack txt
    
    listOfOxNumb txt = checkIfListEmpty . liftM (map DT.pack) . getUnitsNumber ["oxidation number","oxidation numbers"] "" $ return . removelastPuncs . removefirstPuncs $ mapText $ DT.unpack txt
    
    listOfElem txt = checkIfListEmpty . getElements . appendChemResults chemSymb . removelastPuncs . removefirstPuncs $ mapText $ DT.unpack txt



surfaceSymb :: [String]
surfaceSymb = ["cm2","m2","km2","mm2","cm^2","m^2","km^2","mm^2"]

densSymb :: [String]
densSymb = ["g/cm3", "g/cm^3", "g/mL", "kg/l", "kg/L", "lbs/L", "lbs/l"]

massSymb :: [String]
massSymb = ["g", "kg", "mg", "gram", "grams", "L", "mL", "Liters", "liters", "ga", "oz", "ounces"]

subAtoms :: [String]
subAtoms = ["electron", "electrons", "proton", "protons", "neutron", "neutrons"]

pseudoUnits :: [String]
pseudoUnits = ["ppm", "parts-per-million", "ppb", "parts-per-billion", "ppt", "parts-per-trillion", "ppq", "parts-per-quadrillion"]

physConstants :: [String]
physConstants = ["L-atm/mol-deg", "gas constant", "gas constants"]

pressSymb :: [String]
pressSymb = ["atm", "torr"]

tabGroupSymb :: [String]
tabGroupSymb = ["1A", "2A", "3B", "4B", "5B", "6B", "7B", "8", "1B", "2B", "3A", "4A", "5A", "6A", "7A", "8A", "IA", "IIA", "IIIB", "IVB", "VB", "VIB", "VIIB", "VIII", "IB", "IIB", "IIIA", "IVA", "VA", "VIA", "VIIA", "VIIIA"]

chemSymb :: [(String, String)]
chemSymb = ([("H", "hydrogen"), ("He", "helium"),("Li", "lithium"),("Be", "beryllium") ,("B", "boron"),("C", "carbon"),("N", "nitrogen"),("O", "oxygen"),("F", "fluorine"),("Ne", "neon"),("Na", "sodium"),("Mg", "magnesium"),("Al", "alluminium"),("Si", "silicon"),("P", "phosphous"),("S", "sulphur"),("Cl", "chlorine"),("Ar", "argon"),("K", "potassium"),("Ca", "calcium"),("Sc", "scandium"),("Ti", "titanium"),("V", "vanadium"),("Cr", "chromium"),("Mn", "manganese"),("Fe", "iron"),("Co", "cobalt"),("Ni", "nickel"),("Cu", "copper"),("Zn", "zinc"),("Ga", "gallium"),("Ge", "germanium"),("As", "arsenic"),("Se", "selenium"),("Br", "bromine"),("Kr", "krypton"),("Rb", "rubidium"),("Sr", "strontium"),("Y", "yttrium")])




-- finds groups and periods in the periodic table. Finds for example  "group IIIA" or "Period 6" or "group IIA and IIIB"
getFromTable :: Maybe [String] -> [String] -> Maybe [String]
getFromTable (Just (x:y:z:s:xs)) wordsList = case any (==True) . map (==x) $ wordsList of
                                             True -> case (any (isDigit) y || (any (=='A') y || any (=='B') y)) && any (isDigit) s || (any (=='A') s || any (=='B') s) of
                                                          True -> liftM (y:) $ liftM (s :) (getFromTable (liftM (y :) (liftM (z :) (liftM (s :) (Just xs)))) wordsList)
                                                          False -> case any (isDigit) y || (any (=='A') y || any (=='B') y)  of
                                                                        True -> liftM (y :) (getFromTable (liftM (y :) (liftM (z :) (liftM (s :) (Just xs)))) wordsList)
                                                                        _ -> getFromTable (liftM (y :) (liftM (z :) (liftM (s :) $ Just xs))) wordsList
                                             False -> getFromTable (liftM (y :) (liftM (z :) (liftM (s :) $ Just xs))) wordsList
                                             
getFromTable (Just (_:xs)) wordsList = getFromTable (Just xs) wordsList
                                     
getFromTable (Just _) wordsList = Just []                               
getFromTable _ wordsList = Nothing


-- adds to list all keys from Map chemSymb, that were found in text
lookupChemKeys :: [(String,String)] -> [String] -> Maybe [String]
lookupChemKeys list text = if null [key | (key,value) <- list, key `elem` text] then Nothing else Just [key | (key,value) <- list, key `elem` text]

-- adds to list all keys from Map chemSymb, which values were found in text
lookupChemValues :: [(String,String)] -> [String] -> Maybe [String]
lookupChemValues list text = if null [key | (key,value) <- list, value `elem` text] then Nothing else Just [key | (key,value) <- list, value `elem` text]

-- adds two lists with symbols
appendChemResults :: [(String,String)] -> [String] -> Maybe [String]
appendChemResults list text  = mappend (lookupChemKeys list text) (lookupChemValues list text)

-- converts String to Element, to use functions from Radium.Element. See https://hackage.haskell.org/package/radium-0.4/candidate/docs/src/Radium-Element.html#Element
getElements :: Maybe [String] -> Maybe [Element]
getElements (Just x) = Just [elementBySymbol value | value <- x, not.null $ value]
getElements _ = Nothing
