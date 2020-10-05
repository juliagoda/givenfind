# GivenFind module

1. [About module](#about-module)  
  * [Questions - examples](#questions-\--examples)  
  * [Physics - examples](#physics-\--examples)  
  * [Geography - examples](#geography-\--examples)  
  * [Chemistry - examples](#chemistry-\--examples)  
2. [Installation](#installation)  
  * [As project](#as-project)  
  * [Including in other projects](#including-in-other-projects)  
  * [Problems](#problems)  
3. [Tests](#tests)  
4. [Future](#future)  


<br/>
<br/>

## About module

This module provides search functions over texts in two formats - String
or Text from Data.Text module. It is designed to get questions, commands
and data with symbols and units from exercise of different branches. Most of the functions
gets only one parameter - your exercise in String or Text format. If you think, that a list
of units and symbols are not sufficient, you can use functions that take two
parameters - your text and searched symbol or even list of symbols. 

 If there are duplicates of names, I suggest using for example:


import qualified GivenFind.Physics as P  
import qualified GivenFind.Questions as Q  
...


<br/>

### Questions - examples

import GivenFind.Questions  

<br/>


`findFirstQuestion "Suppose there were a mountain of volume M that has in it a (Poisson) distribution of mine-able gold having total volume G, and you are given a mine claim that includes a total mine-able volume V of the mountain. While you are deliberating as to whether or not to invest in the (considerable!) expense of gold-mining, someone else works a claim, mining out a volume of the mountain R that yields a volume Rg of gold. If you were then given the opportunity to trade your mine for another (unworked) mine of equal volume V, anywhere else on the mountain, what should you do? How much can you expect to win if you keep your mine? And if you trade for another one?"`

*Result:    Just " If you were then given the opportunity to trade your mine for another (unworked) mine of equal volume V, anywhere else on the mountain, what should you do?"*

--

`findLastQuestion "Suppose there were a mountain of volume M that has in it a (Poisson) distribution of mine-able gold having total volume G, and you are given a mine claim that includes a total mine-able volume V of the mountain. While you are deliberating as to whether or not to invest in the (considerable!) expense of gold-mining, someone else works a claim, mining out a volume of the mountain R that yields a volume Rg of gold. If you were then given the opportunity to trade your mine for another (unworked) mine of equal volume V, anywhere else on the mountain, what should you do? How much can you expect to win if you keep your mine? And if you trade for another one?"`

*Result:    Just "And if you trade for another one?"*

--

`findQuestions "Suppose there were a mountain of volume M that has in it a (Poisson) distribution of mine-able gold having total volume G, and you are given a mine claim that includes a total mine-able volume V of the mountain. While you are deliberating as to whether or not to invest in the (considerable!) expense of gold-mining, someone else works a claim, mining out a volume of the mountain R that yields a volume Rg of gold. If you were then given the opportunity to trade your mine for another (unworked) mine of equal volume V, anywhere else on the mountain, what should you do? How much can you expect to win if you keep your mine? And if you trade for another one?"`

*Result:    Just [" If you were then given the opportunity to trade your mine for another (unworked) mine of equal volume V, anywhere else on the mountain, what should you do?","How much can you expect to win if you keep your mine?","And if you trade for another one?"]*

--

`ifIsQuestion "A car starts from rest and accelerates uniformly over a time of 5.21 seconds for a distance of 110 m. Determine the acceleration of the car.""`

*Result:    False*

--

`howManyQuestions "Suppose there were a mountain of volume M that has in it a (Poisson) distribution of mine-able gold having total volume G, and you are given a mine claim that includes a total mine-able volume V of the mountain. While you are deliberating as to whether or not to invest in the (considerable!) expense of gold-mining, someone else works a claim, mining out a volume of the mountain R that yields a volume Rg of gold. If you were then given the opportunity to trade your mine for another (unworked) mine of equal volume V, anywhere else on the mountain, what should you do? How much can you expect to win if you keep your mine? And if you trade for another one?"`

*Result:    3*

-- 

`findLastTask "A car starts from rest and accelerates uniformly over a time of 5.21 seconds for a distance of 110 m. Determine the acceleration of the car."`

*Result:    Just " Find acceleration, if time is 10 seconds for a distance of 2.0 m"*

--

`findFirstTask "A car starts from rest and accelerates uniformly over a time of 5.21 seconds for a distance of 110 m. Determine the acceleration of the car. Find acceleration, if time is 10 seconds for a distance of 200 m."`

*Result:      Just " Determine the acceleration of the car"*

--

`findTasks "A car starts from rest and accelerates uniformly over a time of 5.21 seconds for a distance of 110 m. Determine the acceleration of the car. Find acceleration, if time is 10 seconds for a distance of 200 m."`

*Result:    Just [" Determine the acceleration of the car"," Find acceleration, if time is 10 seconds for a distance of 2.0 m"]*

--

`findFirstTask "A car starts from rest and accelerates uniformly over a time of 5.21 seconds for a distance of 110 m. The acceleration of the car is 10m/s2."`

*Result:   Nothing*

--

`ifIsTask "A car starts from rest and accelerates uniformly over a time of 5.21 seconds for a distance of 110 m. Determine the acceleration of the car."`

*Result:    True*

--

`howManyTasks "A car starts from rest and accelerates uniformly over a time of 5.21 seconds for a distance of 110 m. Determine the acceleration of the car. Find acceleration, if time is 10 seconds for a distance of 200 m."`

*Result:    2*

--

`getAll "With what speed in miles/hr (1 m/s = 2.23 mi/hr) must an object be thrown to reach a height of 91.5 m (equivalent to one football field)? Assume negligible air resistance."`

*Result:   Just ["Assume negligible air resistance","With what speed in miles/hr (1 m/s = 2.23 mi/hr) must an object be thrown to reach a height of 91.5 m (equivalent to one football field)?"]*


<br/>

### Physics - examples

import GivenFind.Physics  

<br/>

`searchAllPhys "A car starts from rest and accelerates uniformly over a time of 5.21 seconds for a distance of 110 m. Determine the acceleration of the car."`

*Result:    Symbol [("s","110.0 m"),("t","5.21 seconds")]*

-- 

`searchAllPhys "A car starts from rest and accelerates uniformly over a time of seconds for a distance of m. Determine the acceleration of the car."`

*Result:     None*

--

`searchYourOneSymb "Find the surface area if the length of one side is 3 cm" "cm"`

*Result:   Symbol ("cm",3.0)*

--

`searchAllPhysSymb "Rocket-powered sleds are used to test the human response to acceleration. If a rocket-powered sled is accelerated to a speed of 444 m/s in 1.83 seconds, then what is the acceleration and what is the distance that the sled travels?"`

*Result:    Symbol [("seconds",1.83),("m/s",444.0)]*

--

`searchOnePhysSymb "Rocket-powered sleds are used to test the human response to acceleration. If a rocket-powered sled is accelerated to a speed of 444 m/s in 1.83 seconds, then what is the acceleration and what is the distance that the sled travels?"`

*Result:    Symbol ("seconds",1.83)*

--

`searchYourAllSymb "Find the surface area if the length of one side is 3 cm and second is 1 m" ["cm","m"]`

*Result:   Symbol [("cm",3.0),("m",1.0)]*

--


<br/>

### Geography - examples

import GivenFind.Geography  

<br/>

`listOfTemperatures "The combined global land and ocean average surface temperature for November 2010 was 0.69°C (1.24°F) above the 20th century average of 12.9°C (55.2°F). This was the second warmest such period on record. 2004 was the warmest November on record."`

*Result: Just ["0.69\176C","0.69\176C(1.24\176F)","12.9\176C","12.9\176C(55.2\176F)"]*

--

`listOfHours "The main reason for this is that similar working day schedules around the world have led to people rising on average at 07:00 clock time and going to bed at 23:00 clock time."`

*Result: Just [07:00:00,23:00:00]*

--

`listOfTitudes "A more extreme example is Nome, Alaska, which is at 165°24′W longitude—just west of center of the idealized Samoa Time Zone 165°W."`

*Result: Just [Height (Longitude (Just (-165.24))),Height (Longitude (Just (-165.0)))]*

--

`listOfRadians "The 15° gore that is offset from GMT or UT1 (not UTC) by twelve hours is bisected by the nautical date line into two 7.5° gores that differ from GMT by ±12 hours."`

*Result: Just [0.2617993877991494,0.1308996938995747]*

--

`listOfScales "For example, a global map would probably have a small cartographic scale (e.g. 1:800,000) and a city map would probably have a large cartographic scale (e.g. 1:20,000)."`

*Result:    Just ["1:800,000","1:20,000"]*

--

`listOfLevels "Its elevation is 400 meters below sea level."`

*Result:    Just ["400meters below sea level"]*

--

`listOfTemperatures "Zero Kelvin is also called absolute zero, the coldest temperature and lowest energy level. Absolute zero is equal to about minus -273oC."`

*Result:    Just ["-273oC"]*

--
  
Results with other types than String or Text have sense because of existing types in other modules like "Longitude" or "Latitude". 

--


<br/>

### Chemistry - examples

import GivenFind.Chemistry  

<br/>

`listOfSurfaces "Convert 12 cm2 to m2"`

*Result:    Just ["12cm2"]*

--

`listOfDens "The density of water at 20oC is 0.9982 g/mL. Of the two sets of density measurements, which set is more accurate?"`

*Result:    Just ["0.9982g/mL"]*

--

`listOfMass "Convert 455 mg to kg"`

*Result:    Just ["455mg"]*

--

`listOfSubAtoms "An isotope with 60 protons and 60 neutrons should be stable"`

*Result:    Just ["60protons","60neutrons"]*

--

`listOfPress "Atmospheric pressure in cities that have an altitude of about a mile, such as Denver and Reno, typically have an atmospheric pressure of about 6.4x102 torr. What is the pressure in atmospheres?"`

*Result:    Just ["6.4x102torr"]*

--

`listOfElem "The oxidation number of chromium in dichromate is:"`

*Result:    Just [Cr]*

--

`listOfGroups "The oxidation state of Group VIIA (17) in binary compounds is (except for dihalogen compounds, e.g., ICl):"`

*Result:    Just ["VIIA"]*

--

`listOfMoles "How many moles of HCl are required for complete reaction of 0.40 moles of zinc?"`

*Result:    Just ["0.40moles"]*

--



<br/>
<br/>

## Installation

Remember, that installations work for your projects, not globally. Global installation of modules, that are not from stackage is not recommended.

<br/>

### As project

```
git clone https://github.com/juliagoda/givenfind.git
cd givenfind
stack setup && stack build
```
<br/>

### Including in other projects

Open stack.yaml file in your project directory. Below line with "packages:", add lines with git repository and last commit:  

*\- location:*  <br/>
*git: https://github.com/juliagoda/givenfind.git* <br/>
*commit: 5f1cd3222637b6abc8d0116f04cab5af6d824938* <br/>
  
Below line with "extra-deps:" add lines with external dependencies:  

*\- gps-1.2* <br/>
*\- reverse-geocoding-0.3.0.0* <br/>
*\- authenticate-oauth-1.5.1.2* <br/>
*\- http-client-0.4.31.2* <br/>
*\- http-client-tls-0.2.4.1* <br/>
*\- wreq-0.4.1.0* <br/>
*\- radium-0.8.0* <br/>

install external dependencies with command:  

`stack solver --update-config`

<br/> 

Second method is use a git clone in project (where stack.yaml is):  

```
git clone https://github.com/juliagoda/givenfind.git
```

next step is adding below line with "packages:" in stack.yaml (in main project) with name of downloaded module's directory (where is another .cabal file):  

\- givenfind  
\- '.'   

install external dependencies with command:  

`stack solver --update-config`

<br/>

### Problems

To fix your problems with configuration, installation, creation or update of your project, [check this site](https://docs.haskellstack.org/en/stable/GUIDE/).

<br/>
<br/>

## Tests

Just to test modules classes run the commands (where stack.yaml is):

```
stack test
```

<br/>
<br/>

## Future

It's not over. I'm going to test and add next modules of different branches to this project. Someone can notice that there are lacks of some units or symbols, but all will be improved.
