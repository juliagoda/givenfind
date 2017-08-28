{-# LANGUAGE MultiParamTypeClasses
            ,FlexibleInstances
            ,TypeSynonymInstances
            ,TemplateHaskell #-}
             
module GivenFind.Chemistry  
( 
) where  

import GivenFind
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
import qualified Data.Map as M
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
tabGroupSymb = ["1A", "2A", "3A", "4A", "5A", "6A", "7A", "8A", "IA", "IIA", "IIIA", "IVA", "VA", "VIA", "VIIA", "VIIIA"]
