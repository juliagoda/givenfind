{-# LANGUAGE MultiParamTypeClasses
            ,FlexibleInstances
            ,TypeSynonymInstances
            ,TemplateHaskell #-}
             
module GivenFind.Chemistry  
() where  

import GivenFind
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
