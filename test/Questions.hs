{-# LANGUAGE TemplateHaskell, FlexibleContexts #-}

module Questions(tests) where
 
import Test.QuickCheck
import Samples
import GivenFind.Questions


---------------------- findFirstQuestion :: a -> Maybe a --------------------------------------


prop_testFirstQuestion1 = findFirstQuestion questionSample1 == Just "Does this make any sense?"

prop_testFirstQuestion2 = findFirstQuestion questionSample2 == Just "First things first, what is property-based testing?"

prop_testFirstQuestion3 = findFirstQuestion questionSample3 == Nothing

prop_testFirstQuestion4 = findFirstQuestion questionSample4 == Just "What does the following program print out?"

prop_testFirstQuestion5 = findFirstQuestion questionSample5 == Nothing

prop_testFirstQuestion6 = findFirstQuestion questionSample6 == Nothing

prop_testFirstQuestion7 = findFirstQuestion questionSample7 == Nothing

prop_testFirstQuestion8 = findFirstQuestion questionSample8 == Nothing


---------------------- findLastQuestion :: a -> Maybe a --------------------------------------


prop_testLastQuestion1 = findLastQuestion questionSample1 == Just "Maybe doesn't?"

prop_testLastQuestion2 = findLastQuestion questionSample2 == Just "First things first, what is property-based testing?"

prop_testLastQuestion3 = findLastQuestion questionSample3 == Nothing

prop_testLastQuestion4 = findLastQuestion questionSample4 == Just "And why?"

prop_testLastQuestion5 = findLastQuestion questionSample5 == Nothing

prop_testLastQuestion6 = findLastQuestion questionSample6 == Nothing

prop_testLastQuestion7 = findLastQuestion questionSample7 == Nothing

prop_testLastQuestion8 = findLastQuestion questionSample8 == Nothing


---------------------- findQuestions :: a -> Maybe [a] --------------------------------------


prop_testFindQuestions1 = Just ["Does this make any sense?", "Maybe doesn't?"] == findQuestions questionSample1

prop_testFindQuestions2 = Just ["First things first, what is property-based testing?"] == findQuestions questionSample2

prop_testFindQuestions3 = findQuestions questionSample3 == Nothing

prop_testFindQuestions4 = Just ["What does the following program print out?", "And why?"] == findQuestions questionSample4

prop_testFindQuestions5 = findQuestions questionSample5 == Nothing

prop_testFindQuestions6 = findQuestions questionSample6 == Nothing

prop_testFindQuestions7 = findQuestions questionSample7 == Nothing

prop_testFindQuestions8 = findQuestions questionSample8 == Nothing


---------------------- ifIsQuestion :: a -> Bool --------------------------------------


prop_testifIsQuestion1 = ifIsQuestion questionSample1 == True

prop_testifIsQuestion2 = ifIsQuestion questionSample2 == True

prop_testifIsQuestion3 = ifIsQuestion questionSample3 == False

prop_testifIsQuestion4 = ifIsQuestion questionSample4 == True

prop_testifIsQuestion5 = ifIsQuestion questionSample5 == False

prop_testifIsQuestion6 = ifIsQuestion questionSample6 == False

prop_testifIsQuestion7 = ifIsQuestion questionSample7 == False

prop_testifIsQuestion8 = ifIsQuestion questionSample8 == False


---------------------- howManyQuestions :: a -> Int --------------------------------------


prop_testhowManyQuestions1 = howManyQuestions questionSample1 == 2

prop_testhowManyQuestions2 = howManyQuestions questionSample2 == 1

prop_testhowManyQuestions3 = howManyQuestions questionSample3 == 0

prop_testhowManyQuestions4 = howManyQuestions questionSample4 == 2

prop_testhowManyQuestions5 = howManyQuestions questionSample5 == 0

prop_testhowManyQuestions6 = howManyQuestions questionSample6 == 0

prop_testhowManyQuestions7 = howManyQuestions questionSample7 == 0

prop_testhowManyQuestions8 = howManyQuestions questionSample8 == 0


---------------------- findFirstTask :: a -> Maybe a --------------------------------------


prop_testfindFirstTask1 = findFirstTask taskSample1 == Just "Determine the acceleration of the car."

prop_testfindFirstTask2 = findFirstTask taskSample2 == Just "Find out how many different lunches you could buy without receiving any change back."

prop_testfindFirstTask3 = findFirstTask taskSample3 == Nothing

prop_testfindFirstTask4 = findFirstTask taskSample4 == Nothing

prop_testfindFirstTask5 = findFirstTask taskSample5 == Nothing

prop_testfindFirstTask6 = findFirstTask taskSample6 == Nothing

prop_testfindFirstTask7 = findFirstTask taskSample7 == Just "Using the menu to the left, find out how many different lunches you could buy without receiving any change back."


---------------------- findLastTask :: a -> Maybe a --------------------------------------


prop_testfindLastTask1 = findLastTask taskSample1 == Just "Find acceleration, if time is 10 seconds for a distance of 200 m."

prop_testfindLastTask2 = findLastTask taskSample2 == Just "Find out how many different lunches you could buy without receiving any change back."

prop_testfindLastTask3 = findLastTask taskSample3 == Nothing

prop_testfindLastTask4 = findLastTask taskSample4 == Nothing

prop_testfindLastTask5 = findLastTask taskSample5 == Nothing

prop_testfindLastTask6 = findLastTask taskSample6 == Nothing

prop_testfindLastTask7 = findLastTask taskSample7 == Just "Using the menu to the left, find out how many different lunches you could buy without receiving any change back."


---------------------- findTasks :: a -> Maybe [a] --------------------------------------


prop_testfindTasks1 = findTasks taskSample1 == Just ["Determine the acceleration of the car.", "Find acceleration, if time is 10 seconds for a distance of 200 m."]

prop_testfindTasks2 = findTasks taskSample2 == Just ["Find out how many different lunches you could buy without receiving any change back."]

prop_testfindTasks3 = findTasks taskSample3 == Nothing

prop_testfindTasks4 = findTasks taskSample4 == Nothing

prop_testfindTasks5 = findTasks taskSample5 == Nothing

prop_testfindTasks6 = findTasks taskSample6 == Nothing

prop_testfindTasks7 = findTasks taskSample7 == Just ["Using the menu to the left, find out how many different lunches you could buy without receiving any change back."]


---------------------- ifIsTask :: a -> Bool --------------------------------------


prop_testifIsTask1 = ifIsTask taskSample1 == True

prop_testifIsTask2 = ifIsTask taskSample2 == True

prop_testifIsTask3 = ifIsTask taskSample3 == False

prop_testifIsTask4 = ifIsTask taskSample4 == False

prop_testifIsTask5 = ifIsTask taskSample5 == False

prop_testifIsTask6 = ifIsTask taskSample6 == False

prop_testifIsTask7 = ifIsTask taskSample7 == True


---------------------- howManyTasks :: a -> Int --------------------------------------


prop_testhowManyTasks1 = howManyTasks taskSample1 == 2

prop_testhowManyTasks2 = howManyTasks taskSample2 == 1

prop_testhowManyTasks3 = howManyTasks taskSample3 == 0

prop_testhowManyTasks4 = howManyTasks taskSample4 == 0

prop_testhowManyTasks5 = howManyTasks taskSample5 == 0

prop_testhowManyTasks6 = howManyTasks taskSample6 == 0

prop_testhowManyTasks7 = howManyTasks taskSample7 == 1


---------------------- getAll :: a -> Maybe [a] --------------------------------------


prop_testgetAll1 = getAll questionSample1 == Just ["Does this make any sense?", "Maybe doesn't?"]

prop_testgetAll2 = getAll questionSample2 == Just ["First things first, what is property-based testing?"]

prop_testgetAll3 = getAll questionSample3 == Nothing

prop_testgetAll4 = getAll questionSample4 == Just ["What does the following program print out?", "And why?"]

prop_testgetAll5 = getAll questionSample5 == Nothing

prop_testgetAll6 = getAll questionSample6 == Nothing

prop_testgetAll7 = getAll questionSample7 == Nothing

prop_testgetAll8 = getAll questionSample8 == Nothing

prop_testgetAll9 = getAll taskSample1 == Just ["Determine the acceleration of the car.", "Find acceleration, if time is 10 seconds for a distance of 200 m.", "What will be the result?"]

prop_testgetAll10 = getAll taskSample2 == Just ["Find out how many different lunches you could buy without receiving any change back.", "What will be the result?"]

prop_testgetAll11 = getAll taskSample3 == Nothing

prop_testgetAll12 = getAll taskSample4 == Nothing

prop_testgetAll13 = getAll taskSample5 == Nothing

prop_testgetAll14 = getAll taskSample6 == Nothing

prop_testgetAll15 = getAll taskSample7 == Just ["Using the menu to the left, find out how many different lunches you could buy without receiving any change back."]



return []
tests = $quickCheckAll
