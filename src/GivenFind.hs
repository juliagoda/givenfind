{- |
   Module     : GivenFind
   License    : GNU GENERAL PUBLIC LICENSE
   Maintainer : Jagoda "juliagoda" Górska <juliagoda.de@gmail.com>
   Portability: portable

Written by Jagoda "juliagoda" Górska,  juliagoda.de@gmail.com

Please start with the introduction at "GivenFind#intro".
-}

module GivenFind where
    
import Prelude
import Control.Monad
import qualified Data.Text as T
import GivenFind.Base
import GivenFind.Physics

{- $intro
 #intro#
Welcome to GivenFind.

This module provides search functions over texts in two formats - String
or Text from Data.Text module. It is designed to get questions, commands
and data with symbols and units from exercise of different branches. Most of the functions
gets only one parameter - your exercise in String or Text format. If you think, that a list
of units and symbols are not sufficient, you can use functions that take two
parameters - your text and searched symbol or even list of symbols. 

 If there are duplicates of names, I suggest using:

>import qualified GivenFind.Physics as P
>import qualified GivenFind.Base as G

-}
