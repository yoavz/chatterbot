module Chatterbot
( chatterbot 
) where

import Data.List (find)
import Data.Maybe 
import Data.Char (toLower)
import System.Random

import Useful
import Part3

-- TYPES
type Phrase = [String]
type PhrasePair = (Phrase, Phrase)
type BotBrain = [(Phrase, [Phrase])]

reflections =
  [ ("am",     "are"),
    ("was",    "were"),
    ("i",      "you"),
    ("i'm",    "you are"),
    ("i'd",    "you would"),
    ("i've",   "you have"),
    ("i'll",   "you will"),
    ("my",     "your"),
    ("me",     "you"),
    ("are",    "am"),
    ("you're", "i am"),
    ("you've", "i have"),
    ("you'll", "i will"),
    ("your",   "my"),
    ("yours",  "mine"),
    ("you",    "me")
  ]

reflectAux :: String -> String
reflectAux word 
    | isJust result = snd . fromJust $ result
    | otherwise = word
    where result = find ((== word) . fst) reflections
reflect :: Phrase -> Phrase
reflect = map reflectAux 

rulesApply :: [PhrasePair] -> Phrase -> Phrase
rulesApply = try . (transformationsApply "*" reflect)

stateOfMind :: BotBrain -> IO (Phrase -> Phrase)
stateOfMind brain = do
    r <- randomIO :: IO Float
    let randomElem list = list !! (floor . (*r) . fromIntegral . length $ list)
        rules = map (map2 (id, randomElem)) brain
        in return (rulesApply rules)

endOfDialog = (=="quit") . map toLower
present = unwords
prepare = words . map toLower . filter (not . flip elem ".,:;!#%&|")

rulesCompile :: [(String, [String])] -> BotBrain
rulesCompile = 
    let lowerWords = words . map toLower
    in map (map2 (prepare, map words))

-- MAIN LOOP
chatterbot :: String -> [(String, [String])] -> IO ()
chatterbot botName botRules = do
    putStrLn ("\n\nHi! I am " ++ botName 
                              ++ ". How are you?")
    botloop
  where
    brain = rulesCompile botRules
    botloop = do
      putStr "\n: "
      question <- getLine
      answer <- stateOfMind brain
      putStrLn (botName ++ ": " 
         ++ (present . answer . prepare) question)
      if (not . endOfDialog) question 
         then botloop 
         else return ()

-- TESTS 
testReflect1 = (==) (reflect ["i've"]) ["you have"]
testReflect2 = (==) (reflect ["i've", "are", "your"]) ["you have", "am", "my"]
testReflect3 = (==) (reflect ["i", "will", "never", "see", "my",
                              "reflection", "in", "your", "eyes"])
                             ["you", "will", "never", "see", "your",
                              "reflection", "in", "my", "eyes"]
testReflect = and [testReflect1, testReflect2, testReflect3]

testRules = [(words "My name is *", words "Hello *"), (words "I like *", words "Why do you like *")]
testRulesX = rulesApply testRules
testRulesApply1 = (==) (testRulesX (words "My name is yoav")) (words "Hello yoav")
testRulesApply2 = (==) (testRulesX (words "I like turtles")) (words "Why do you like turtles")
testRulesApply3 = (==) (testRulesX (words "I like me")) (words "Why do you like you")
testRulesApply4 = (==) (testRulesX (words "No match!")) (words "No match!") 
testRulesApply = and [testRulesApply1, testRulesApply2, testRulesApply3, testRulesApply4]

testChatterbot = and [testReflect, testRulesApply]
