module Pattern where
import Utilities
import Data.Maybe 
import Data.List (find)

-------------------------------------------------------
-- Match and substitute
--------------------------------------------------------

-- Replaces a wildcard in a list with the list given as the third argument
-- Implementation note: It was possible to remove more arguments to make
--                      this function even more pointfree, but I believe
--                      this form is more readable

substitute :: Eq a => a -> [a] -> [a] -> [a]
substitute _ [] _ = []
substitute w (x:xs) y
    | x == w = y ++ (substitute w xs y)
    | otherwise = x : (substitute w xs y) 

-- Helper function to match
-- Note: I added a wildcard argument to the definition of these functions due 
--       to the case where the rest of the match includes a wildcard and 
--       we need to call match recursively to find what it is 
--       Example:
--       singleWildcardMatch '*' '*word*' 'aworda' = Just 'a'
--       longerWildcardMatch '*' '*rd*' 'aworda' = Just 'awo'

--       I also experimented with several pointfree solutions to these functions
--       but found that the following versions were much more readable in my opinion

singleWildcardMatch, longerWildcardMatch :: Eq a => a -> [a] -> [a] -> Maybe [a]
singleWildcardMatch w (x:xs) (y:ys) = mmap (const [y]) (match w xs ys) 
longerWildcardMatch w (x:xs) y = mmap snd rMatch  
    where rMatch = find (isJust . (match w xs) . fst) [((drop n y), (take n y)) | n <- [1..(length y)]]

-- Tries to match two lists. If they match, the result consists of the sublist
-- bound to the wildcard in the pattern list.
-- Implementation note: Again, I tried several options to make match pointfree, 
--                      but I found that this implementation was the most readable
--                      form I came up with.

match :: Eq a => a -> [a] -> [a] -> Maybe [a]
match _ [] [] = Just []
match _ [] _ = Nothing 
match _ _ [] = Nothing 
match w p s
    | head p == w = orElse (singleWildcardMatch w p s) (longerWildcardMatch w p s)
    | head p == head s = match w (tail p) (tail s)
    | otherwise = Nothing

-- Test cases --------------------

testPattern =  "a=*;"
testSubstitutions = "32"
testString = "a=32;"

substituteTest = substitute '*' testPattern testSubstitutions
substituteCheck = substituteTest == testString

matchTest = match '*' testPattern testString
matchCheck = matchTest == Just testSubstitutions

-- Custom tests --------------------

testSubX = substitute 'x'
testSubstitute1 = (==) (testSubX "3*cos(x) + 4 - x" "5.37") "3*cos(5.37) + 4 - 5.37"
testSubstitute2 = (==) (testSubX "3*cos(x) + 4" "5.37") "3*cos(5.37) + 4"
testSubstitute3 = (==) (testSubX "3*cos(4) + 4" "5.37") "3*cos(4) + 4"
testSubstitute4 = (==) (testSubX "xxx" "Hello") "HelloHelloHello"
testSubstitute = and [testSubstitute1, testSubstitute2, testSubstitute3, testSubstitute4, substituteCheck]

-- match
testDo = match '*' "*do" 
testMatch1 = (==) (testDo "bdo") (Just "b")
testMatch2 = (==) (testDo "dobedo") (Just "dobe")
testMatch3 = (==) (testDo "bedobe") Nothing 
testMatch4 = (==) (match 'x' "2*x+3" "2*7+3") (Just "7") 
testMatch5 = (==) (match '*' "frodo" "gandalf") Nothing 
testMatch6 = (==) (match 2 [1,3..5] [1,3..5]) (Just [])
testMatch7 = (==) (match '*' "* and *" "you and me") (Just "you")
testMatch8 = (==) (match 'x' "2*x+3+x" "2*7+3") (Nothing)
testMatch9 = (==) (match 'x' "x" "") (Nothing)
testMatch = and [testMatch1, testMatch2, testMatch3, testMatch4, testMatch5, testMatch6, testMatch7, testMatch8, testMatch9, matchCheck]

testPart2 = and [testSubstitute, testMatch]

-------------------------------------------------------
-- Applying patterns
--------------------------------------------------------

-- Again, I experimented with some pointfree styles for the 
-- following functions, but the large amount of flips resulted
-- in unreadable code (in my opinion). The following is the most
-- readable and functional style I could find (in my opinion).

-- Applying a single pattern
transformationApply :: Eq a => a -> ([a] -> [a]) -> [a] -> ([a], [a]) -> Maybe [a]
transformationApply w f list pattern =
    mmap (substitute w (snd pattern) . f) (match w (fst pattern) list)

-- Applying a list of patterns until one succeeds
transformationsApply :: Eq a => a -> ([a] -> [a]) -> [([a], [a])] -> [a] -> Maybe [a]
transformationsApply _ _ [] _ = Nothing
transformationsApply w f (p:ps) list =
    orElse (transformationApply w f list p) (transformationsApply w f ps list)

-- Custom tests --------------------

french = ("My name is *", "Je m'appelle *") 
applyX = transformationApply '*' id
testApply1 = (==) (applyX "My name is Zacharias" french) (Just "Je m'appelle Zacharias")
testApply2 = (==) (applyX "My name nope Zacharias" french) (Nothing)
testApply3 = (==) (applyX "My name is  " french) (Just "Je m'appelle  ") 
testApply4 = (==) (transformationApply '*' (\x -> x ++ x) "My name is Zacharias" french) (Just "Je m'appelle ZachariasZacharias")
testApply = and [testApply1, testApply2, testApply3, testApply4]

transforms = [("a: *", "b: *"), ("My name is *", "*"), ("* night", "* tov")]
applysX = transformationsApply '*' id transforms
testApplys1 = (==) (applysX "a: blah") (Just "b: blah")
testApplys2 = (==) (applysX "My name is jonas") (Just "jonas")
testApplys3 = (==) (applysX "GOOD night") (Just "GOOD tov")
testApplys4 = (==) (applysX "This doesn't match") Nothing
testApplys5 = (==) (applysX "") Nothing
testApplys = and [testApplys1, testApplys2, testApplys3, testApplys4, testApplys5]

testPart3 = and [testApply, testApplys]
