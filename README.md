# GivenFind module

1. [About module](#about-module)  
  * [Questions - examples](#questions-\--examples)  
  * [Physics - examples](#physics-\--examples)  
  * [Geography - examples](#geography-\--examples)  
2. [Installation](#installation)  
  * [As project](#as-project)  
  * [Including in other projects](#including-in-other-projects)  
  * [Problems](#problems)  
3. [Future](#future)  


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


<br/>

### Questions - examples

import GivenFind.Questions  

<br/>


`findFirstTask "A car starts from rest and accelerates uniformly over a time of 5.21 seconds for a distance of 110 m. Determine the acceleration of the car."`

*Result:      Just " Determine the acceleration of the car"*

--

`findFirstTask "A car starts from rest and accelerates uniformly over a time of 5.21 seconds for a distance of 110 m. The acceleration of the car is 10m/s2."`

*Result:   Nothing*

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
  
Results with other types than String or Text have sense because of existing types in other modules like "Longitude" or "Latitude". 


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
*commit: 19c9450e2248b0b89a36b1bf8ab51af356142c89* <br/>
  
Below line with "extra-deps:" add lines with external dependencies:  

*\- gps-1.2* <br/>
*\- reverse-geocoding-0.3.0.0* <br/>
*\- authenticate-oauth-1.5.1.2* <br/>
*\- http-client-0.4.31.2* <br/>
*\- http-client-tls-0.2.4.1* <br/>
*\- wreq-0.4.1.0* <br/>

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

## Future

It's not over. I'm going to test and add next modules of different branches to this project. Someone can notice that there are lacks of some units or symbols, but all will be improved.
