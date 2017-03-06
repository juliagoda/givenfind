{-# LANGUAGE ScopedTypeVariables
            ,MultiParamTypeClasses
            ,FunctionalDependencies
            ,FlexibleInstances
            ,BangPatterns
            ,FlexibleContexts
            ,CPP #-}

            
module GivenFind.Base   
( SearchInText(..)
) where
    

    
import qualified Data.List as L
import qualified Data.Text as T
import Data.Char
import Control.Monad
import Data.Maybe


-- class for search questions and tasks
class SearchInText a where
    
    -- finds first question in text
    findFirstQuestion :: a -> Maybe a
    
    -- finds last question in text
    findLastQuestion :: a -> Maybe a
    
    -- finds all questions in text
    findQuestions :: a -> Maybe [a]
    
    -- checks if there is a question in text
    ifIsQuestion :: a -> Bool
    
    -- shows how many questions are in a text
    howManyQuestions :: a -> Int
    
    -- find first task from text
    findFirstTask :: a -> Maybe a
    
    -- find last task from text
    findLastTask :: a -> Maybe a
    
    -- finds all tasks from text
    findTasks :: a -> Maybe [a]
    
    -- checks if there is a task in text
    ifIsTask :: a -> Bool
    
    -- shows how many tasks are in a text
    howManyTasks :: a -> Int

    -- gets questions and tasks from text
    getAll :: a -> Maybe [a]
    

instance SearchInText T.Text where
    
    findFirstQuestion txt = ifCanGetHead . stringToText . checkIfListEmpty . listCorQuestSentences . textToString . mixSplitAndReturn $ txt
    
    findLastQuestion txt = ifCanGetLast . stringToText . checkIfListEmpty . listCorQuestSentences . textToString . mixSplitAndReturn $ txt
    
    findQuestions txt = stringToText . checkIfListEmpty . listCorQuestSentences . textToString . mixSplitAndReturn $ txt

    ifIsQuestion txt = fromJust $ hh ["?"] $ T.unpack txt
    
    howManyQuestions txt = fromJust . liftM length . checkIfListEmpty . listCorQuestSentences . textToString . mixSplitAndReturn $ txt
    
    findFirstTask txt = ifCanGetHead . stringToText . checkIfListEmpty . listCorComSentences . textToString . mixSplitAndReturn $ txt
    
    findLastTask txt = ifCanGetLast . stringToText . checkIfListEmpty . listCorComSentences . textToString . mixSplitAndReturn $ txt
    
    findTasks txt = stringToText . checkIfListEmpty . listCorComSentences . textToString . mixSplitAndReturn $ txt
    
    ifIsTask txt = fromJust $ hh listWordString $ T.unpack txt
    
    howManyTasks txt = fromJust . liftM length . checkIfListEmpty . listCorComSentences . textToString . mixSplitAndReturn $ txt
    
    getAll txt = stringToText . checkIfListEmpty . getAllCommands . textToString . mixSplitAndReturn $ txt
    
    
instance SearchInText String where
    
    findFirstQuestion txt = ifCanGetHead . checkIfListEmpty . listCorQuestSentences . textToString . mixSplitAndReturn $ T.pack txt
    
    findLastQuestion txt = ifCanGetLast . checkIfListEmpty . listCorQuestSentences . textToString . mixSplitAndReturn $ T.pack txt
    
    findQuestions txt = checkIfListEmpty . listCorQuestSentences . textToString . mixSplitAndReturn $ T.pack txt

    ifIsQuestion txt = fromJust $ hh ["?"] txt
    
    howManyQuestions txt = fromJust . liftM length . checkIfListEmpty . listCorQuestSentences . textToString . mixSplitAndReturn $ T.pack txt
    
    findFirstTask txt = ifCanGetHead . checkIfListEmpty . listCorComSentences . textToString . mixSplitAndReturn $ T.pack txt
    
    findLastTask txt = ifCanGetLast . checkIfListEmpty . listCorComSentences . textToString . mixSplitAndReturn $ T.pack txt
    
    findTasks txt = checkIfListEmpty . listCorComSentences . textToString . mixSplitAndReturn $ T.pack txt
    
    ifIsTask txt = fromJust $ hh listWordString txt
    
    howManyTasks txt = fromJust . liftM length . checkIfListEmpty . listCorComSentences . textToString . mixSplitAndReturn $ T.pack txt
    
    getAll txt = checkIfListEmpty . getAllCommands . textToString . mixSplitAndReturn $ T.pack txt

    
    
 -- list of words in task (needed when there are no questions)   
data CommandWords = Find | Fill | Solve | Calculate | Complete | Use | Round | Substract | Graph | Evaluate | Move | Consider | Write | Assume | Determine | Perform | Express | Change | Assuming deriving (Show)

-- list of words, that mostly are written in text one place after dot character
listWords :: [CommandWords]
listWords = [Find, Fill, Solve, Calculate, Complete, Use, Round, Substract, Graph, Evaluate, Move, Consider, Write, Assume, Determine, Perform, Express, Change, Assuming]

-- conversion from CommandWords data type to String
listWordString :: [String]
listWordString = [show c | c <- listWords]

-- converts dot to coma, if there is a number one place after dot character and one place before dot character
convertDotToComa ::  Char -> Char -> T.Text -> Int -> T.Text -> Maybe T.Text
convertDotToComa charFrom charTo newText count text = case count <= (length (T.unpack text)) - 1 of 
                                                                                    True -> case T.index text count of 
                                                                                                charFrom -> if (isDigit (T.index text (secureTextLength (count - 1) ((length (T.unpack text)) - 1)))) && (isDigit (T.index text (secureTextLength (count + 1) ((length (T.unpack text)) - 1)))) then convertDotToComa charFrom charTo (T.snoc newText charTo) (count + 1) text else  convertDotToComa charFrom charTo (T.snoc newText (T.index text count)) (count + 1) text
                                                                                    False -> Just newText

-- adds dot character one place after question mark
addDotAfterQuest ::  T.Text -> Int -> T.Text -> Maybe T.Text
addDotAfterQuest newText count text = case count <= (length (T.unpack text)) - 1 of 
                                                                                    True -> if (T.index text (secureTextLength (count - 1) ((length (T.unpack text)) - 1)) == '?') then addDotAfterQuest (T.snoc newText '.') (count + 1) text else addDotAfterQuest (T.snoc newText (T.index text count)) (count + 1) text
                                                                                    False -> Just newText

-- secure function to prevent of stack overflow appearing, if searches Char behind or after text
secureTextLength :: Int -> Int -> Int 
secureTextLength count lngth
    | count < 0 = 0
    | count > lngth = lngth
    | otherwise = count

    
-- after transformations splits text to sentences, thanks to dot characters                                 
splitToSentences :: T.Text -> Maybe [T.Text]
splitToSentences text = (addDotAfterQuest T.empty 0 >=> convertDotToComa '.' ',' T.empty 0) text >>= \b -> return $ T.split (=='.') b

-- in splitted text change coma characters to dot, because getting numbers and their conversion from Text to Num is easier
returnConvert :: Maybe [T.Text] -> Maybe [T.Text]
returnConvert text = text >>= \b -> mapM (convertDotToComa ',' '.' T.empty 0) b

-- binds together two functions to take less place for writing commands
mixSplitAndReturn :: T.Text -> Maybe [T.Text]
mixSplitAndReturn text = returnConvert . splitToSentences $ text

-- prints monad list of sentences, which have one from command words
getAllCommands :: Maybe [String] -> Maybe [String]
getAllCommands texts = do 
    commands <- listCorComSentences texts
    quests <- listCorQuestSentences texts
    return (commands ++ quests)

-- checks, whether needed words have been found as substring in text elements of a list and shows them
listCorComSentences :: Maybe [String] -> Maybe [String]
listCorComSentences texts = do 
    text <- texts     
    filterM (hh listWordString) text

    
-- converts Maybe [T.Text] to Maybe [String]
textToString :: Maybe [T.Text] -> Maybe [String]
textToString texts = texts >>= \b -> return $ map T.unpack b

-- converts Maybe [String] to Maybe [T.Text]
stringToText :: Maybe [String] -> Maybe [T.Text]
stringToText texts = texts >>= \b -> return $ map T.pack b

-- checks if elements from [String] list are in a text. Maybe monad is needed for filterM
hh :: [String] -> String -> Maybe Bool
hh (x:xs) text = if L.isInfixOf x text then Just True else hh xs text
hh _ text = Just False

-- checks, whether needed question marks have been found as substring in text elements of a list and shows them
listCorQuestSentences :: Maybe [String] -> Maybe [String]
listCorQuestSentences texts = do
    text <- texts
    filterM (hh ["?"]) text
    
checkIfListEmpty :: Maybe [a] -> Maybe [a]
checkIfListEmpty list
    | (length (fromJust list)) > 0 = list
    | otherwise = Nothing
    
ifCanGetHead :: Maybe [a] -> Maybe a
ifCanGetHead list = if (length list) > 0 then liftM head list else Nothing

ifCanGetLast :: Maybe [a] -> Maybe a
ifCanGetLast list = if (length list) > 0 then liftM last list else Nothing
