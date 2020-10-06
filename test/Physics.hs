{-# LANGUAGE TemplateHaskell, FlexibleContexts #-}

module Physics(tests) where
 
import Test.QuickCheck
import Samples
import GivenFind.Physics
import GivenFind (Symbols (Symbol, None))


---------------------- searchYourOneSymb :: (SearchPhySymbols s s) => s -> s -> Symbols (s, Double) --------------------------------------


prop_searchYourOneSymb1 = searchYourOneSymb taskPhysicsSample1 "°" == Symbol ("°", 56)

prop_searchYourOneSymb2 = searchYourOneSymb taskPhysicsSample2 "n1" == Symbol ("n1", 1.1)

prop_searchYourOneSymb3 = searchYourOneSymb taskPhysicsSample3 "Hertz" == Symbol ("Hertz", 1014)

prop_searchYourOneSymb4 = searchYourOneSymb taskPhysicsSample4 "m/s" == Symbol ("m/s", 15)

prop_searchYourOneSymb5 = searchYourOneSymb taskPhysicsSample5 "seconds" == Symbol ("seconds", 10)

prop_searchYourOneSymb6 = searchYourOneSymb taskPhysicsSample6 "km/h" == Symbol ("km/h", 25)

prop_searchYourOneSymb7 = searchYourOneSymb taskPhysicsSample7 "mph" == Symbol ("mph", 12)

prop_searchYourOneSymb8 = searchYourOneSymb taskPhysicsSample8 "meters" == Symbol ("meters", 1.0)

prop_searchYourOneSymb9 = searchYourOneSymb taskPhysicsSample9 "nm" == Symbol ("nm", 532)

prop_searchYourOneSymb10 = searchYourOneSymb taskPhysicsSample10 "m" == Symbol ("m", 50.0)

prop_searchYourOneSymb11 = searchYourOneSymb taskPhysicsSample11 "m" == None


---------------------- searchYourAllSymb :: (SearchPhySymbols s) => s -> [s] -> Symbols [(s, Double)] --------------------------------------


prop_searchYourAllSymb1 = searchYourAllSymb taskPhysicsSample1 ["°"] == Symbol [("°", 56)]

prop_searchYourAllSymb2 = searchYourAllSymb taskPhysicsSample2 ["n1", "n2"] == Symbol [("n1", 1.1), ("n2", 1.0)]

prop_searchYourAllSymb3 = searchYourAllSymb taskPhysicsSample3 ["Hertz"] == Symbol [("Hertz", 1014)]

prop_searchYourAllSymb4 = searchYourAllSymb taskPhysicsSample4 ["m/s"] == Symbol [("m/s", 15)]

prop_searchYourAllSymb5 = searchYourAllSymb taskPhysicsSample5 ["seconds", "s"] == Symbol [("seconds", 10), ("s", 15)]

prop_searchYourAllSymb6 = searchYourAllSymb taskPhysicsSample6 ["km/h"]  == Symbol [("km/h", 25), ("km/h", 50)]

prop_searchYourAllSymb7 = searchYourAllSymb taskPhysicsSample7 ["mph"] == Symbol [("mph", 12)]

prop_searchYourAllSymb8 = searchYourAllSymb taskPhysicsSample8 ["minutes", "cm/min"] == Symbol [("minutes", 15), ("cm/min", 60)]

prop_searchYourAllSymb9 = searchYourAllSymb taskPhysicsSample9 ["nm"] == Symbol [("nm", 532)]

prop_searchYourAllSymb10 = searchYourAllSymb taskPhysicsSample10 ["m", "mL"] == Symbol [("m", 50.0), ("mL", 10)]

prop_searchYourAllSymb11 = searchYourAllSymb taskPhysicsSample11 ["m", "mL"] == None


---------------------- searchOnePhysSymb :: (SearchPhySymbols s) => s -> Symbols (s, Double) --------------------------------------


prop_searchOnePhysSymb1 = searchOnePhysSymb taskPhysicsSample1 == Symbol ("°", 56)

prop_searchOnePhysSymb2 = searchOnePhysSymb taskPhysicsSample2 == Symbol ("n1", 1.1)

prop_searchOnePhysSymb3 = searchOnePhysSymb taskPhysicsSample3 == Symbol ("Hertz", 1014)

prop_searchOnePhysSymb4 = searchOnePhysSymb taskPhysicsSample4 == Symbol ("m/s", 15)

prop_searchOnePhysSymb5 = searchOnePhysSymb taskPhysicsSample5 == Symbol ("seconds", 10)

prop_searchOnePhysSymb6 = searchOnePhysSymb taskPhysicsSample6 == Symbol ("km/h", 25)

prop_searchOnePhysSymb7 = searchOnePhysSymb taskPhysicsSample7 == Symbol ("mph", 12)

prop_searchOnePhysSymb8 = searchOnePhysSymb taskPhysicsSample8 == Symbol ("meters", 1.0)

prop_searchOnePhysSymb9 = searchOnePhysSymb taskPhysicsSample9 == Symbol ("nm", 532)

prop_searchOnePhysSymb10 = searchOnePhysSymb taskPhysicsSample10 == Symbol ("m", 50.0)

prop_searchOnePhysSymb11 = searchOnePhysSymb taskPhysicsSample11 == None


---------------------- searchAllPhysSymb :: (SearchPhySymbols s) => s -> Symbols [(s, Double)] --------------------------------------


prop_searchAllPhysSymb1 = searchAllPhysSymb taskPhysicsSample1 == Symbol [("°", 56)]

prop_searchAllPhysSymb2 = searchAllPhysSymb taskPhysicsSample2 == Symbol [("n1", 1.1), ("n2", 1.0)]

prop_searchAllPhysSymb3 = searchAllPhysSymb taskPhysicsSample3 == Symbol [("Hertz", 1014)]

prop_searchAllPhysSymb4 = searchAllPhysSymb taskPhysicsSample4 == Symbol [("m/s", 15)]

prop_searchAllPhysSymb5 = searchAllPhysSymb taskPhysicsSample5 == Symbol [("seconds", 10), ("s", 15)]

prop_searchAllPhysSymb6 = searchAllPhysSymb taskPhysicsSample6 == Symbol [("km/h", 25), ("m", 100), ("km/h", 50)]

prop_searchAllPhysSymb7 = searchAllPhysSymb taskPhysicsSample7 == Symbol [("mph", 12)]

prop_searchAllPhysSymb8 = searchAllPhysSymb taskPhysicsSample8 == Symbol [("meters", 1.0), ("minutes", 15), ("cm/min", 60)]

prop_searchAllPhysSymb9 = searchAllPhysSymb taskPhysicsSample9 == Symbol [("nm", 532)]

prop_searchAllPhysSymb10 = searchAllPhysSymb taskPhysicsSample10 == Symbol [("m", 50.0), ("mL", 10), ("°C", 4), ("°C", 27)]

prop_searchAllPhysSymb11 = searchAllPhysSymb taskPhysicsSample11 == None


---------------------- searchAllPhys :: (SearchPhySymbols s) => s -> Symbols [(s, s)] --------------------------------------


prop_searchAllPhys1 = searchAllPhys taskPhysicsSample1 == Symbol [("angle", "56 °")]

prop_searchAllPhys2 = searchAllPhys taskPhysicsSample2 == Symbol [("n", "1.1 n1"), ("n", "1.0 n2")]

prop_searchAllPhys3 = searchAllPhys taskPhysicsSample3 == Symbol [("Hz", "1014 Hertz")]

prop_searchAllPhys4 = searchAllPhys taskPhysicsSample4 == Symbol [("v", "15 m/s")]

prop_searchAllPhys5 = searchAllPhys taskPhysicsSample5 == Symbol [("t", "10 seconds"), ("t", "15 s")]

prop_searchAllPhys6 = searchAllPhys taskPhysicsSample6 == Symbol [("v", "25 km/h"), ("s","100 m"), ("v", "50 km/h")]

prop_searchAllPhys7 = searchAllPhys taskPhysicsSample7 == Symbol [("v", "12 mph")]

prop_searchAllPhys8 = searchAllPhys taskPhysicsSample8 == Symbol [("s", "1.0 meters"), ("t", "15 minutes"), ("v", "60 cm/min")]

prop_searchAllPhys9 = searchAllPhys taskPhysicsSample9 == Symbol [("nm", "532 nm")]

prop_searchAllPhys10 = searchAllPhys taskPhysicsSample10 == Symbol [("s", "50.0 m"), ("mL", "10 mL"), ("°C", "4 °C"), ("°C", "27 °C")]

prop_searchAllPhys11 = searchAllPhys taskPhysicsSample11 == None


return []
tests = $quickCheckAll
