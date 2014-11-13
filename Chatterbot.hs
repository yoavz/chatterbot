import Useful
import Data.List (find)
import Data.Maybe 
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
rulesApply patterns input = try (transformationsApply "*" reflect patterns) input

-- MAIN LOOP
-- chatterbot :: String -> [(String, [String])] -> IO ()
-- chatterbot botName botRules = do
--     putStrLn ("\n\nHi! I am " ++ botName 
--                               ++ ". How are you?")
--     botloop
--   where
--     brain = rulesCompile botRules
--     botloop = do
--       putStr "\n: "
--       question <- getLine
--
--       answer <- stateOfMind brain
--       putStrLn (botName ++ ": " 
--          ++ (present . answer . prepare) question)
--       if (not . endOfDialog) question 
--          then botloop 
--          else return ()

-- TESTS 
testReflect1 = (==) (reflect ["i've"]) ["you have"]
testReflect2 = (==) (reflect ["i've", "are", "your"]) ["you have", "am", "my"]
testReflect3 = (==) (reflect ["i", "will", "never", "see", "my",
                              "reflection", "in", "your", "eyes"])
                             ["you", "will", "never", "see", "your",
                              "reflection", "in", "my", "eyes"]
testReflect = and [testReflect1, testReflect2, testReflect3]

testRulesRaw = [("My name is *", "Hello *"), ("I like *", "Why do you like *")]
testRules = map (map2 (words, words)) testRulesRaw
testRulesX = rulesApply testRules
testRulesApply1 = (==) (testRulesX (words "My name is yoav")) (words "Hello yoav")
testRulesApply2 = (==) (testRulesX (words "I like turtles")) (words "Why do you like turtles")
testRulesApply3 = (==) (testRulesX (words "I like me")) (words "Why do you like you")
testRulesApply = and [testRulesApply1, testRulesApply2, testRulesApply3]

testChatterbot = and [testReflect, testRulesApply]
