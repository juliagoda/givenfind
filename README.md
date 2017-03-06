# GivenFind module

## Installation

Go to the main directory, where Setup.hs is and write those commands:

**1. cabal configure**
**2. cabal build**
**3. cabal install**


## About module

This module provides search functions over texts in two formats - String
or Text from Data.Text module. It is designed to get questions, commands
and data with symbols and units from exercise of different branches. Most of the functions
gets only one parameter - your exercise in String or Text format. If you think, that a list
of units and symbols are not sufficient, you can use functions that take two
parameters - your text and searched symbol or even list of symbols. 

 If there are duplicates of names, I suggest using:


import qualified GivenFind.Physics as P
import qualified GivenFind.Base as G


## Examples

import GivenFind.Physics


searchAllPhys "A car starts from rest and accelerates uniformly over a time of 5.21 seconds for a distance of 110 m. Determine the acceleration of the car."

*Result:    Symbol [("s","110.0 m"),("t","5.21 seconds")]*

-- 

searchAllPhys "A car starts from rest and accelerates uniformly over a time of seconds for a distance of m. Determine the acceleration of the car."

*Result:     None*

--

findFirstTask "A car starts from rest and accelerates uniformly over a time of 5.21 seconds for a distance of 110 m. Determine the acceleration of the car."

*Result:      Just " Determine the acceleration of the car"*

--

findFirstTask "A car starts from rest and accelerates uniformly over a time of 5.21 seconds for a distance of 110 m. The acceleration of the car is 10m/s2."

*Result:   Nothing*

--

getAll "With what speed in miles/hr (1 m/s = 2.23 mi/hr) must an object be thrown to reach a height of 91.5 m (equivalent to one football field)? Assume negligible air resistance."

*Result   Just ["Assume negligible air resistance","With what speed in miles/hr (1 m/s = 2.23 mi/hr) must an object be thrown to reach a height of 91.5 m (equivalent to one football field)?"]*

--

searchYourOneSymb "Find the surface area if the length of one side is 3 cm" "cm"

*Result:   Symbol ("cm",3.0)*


## Future

It's not over. I'm going to test and add next modules of different branches to this project. Someone can notice that there are lacks of some units or symbols, but all will be improved.
