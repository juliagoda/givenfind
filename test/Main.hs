module Main where
 
import Test.QuickCheck
import GivenFind
import Chemistry
import Geography
import Physics
import Questions


main :: IO Bool
main = Questions.tests 
    >> Physics.tests
