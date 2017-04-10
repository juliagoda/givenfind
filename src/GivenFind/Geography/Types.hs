
module GivenFind.Geography.Types  
( Titudes(..)
) where  

import Data.Geolocation.Reverse.Types as T

data Titudes = Width T.Latitude | Height T.Longitude | None deriving (Show, Read, Eq)
