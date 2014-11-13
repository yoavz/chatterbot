module Part2
( substitute
, match
) where 

import Useful
import Data.Maybe
import Data.List (find)

-- substitute 
substitute :: Eq a => a -> [a] -> [a] -> [a]
substitute _ [] _ = []
substitute wildcard pattern substitution = 
    foldr (\x acc -> if x == wildcard then substitution ++ acc else x:acc) [] pattern


-- match (w)ildcard (p)attern (s)entence 
match :: Eq a => a -> [a] -> [a] -> Maybe [a]
match _ [] [] = Just []
match _ [] _ = Nothing 
match _ _ [] = Nothing 
match w pattern@(p:ps) sentence@(t:ts)
    | p == w = orElse (singleMatch pattern sentence) (longerMatch pattern sentence)
    | p == t = match w ps ts
    | otherwise = Nothing
    where singleMatch (x:xs) (y:ys) 
              | isJust (match w xs ys) = Just [y]
              | otherwise = Nothing
          longerMatch (x:xs) y 
              | isJust rMatch = Just ((snd . fromJust) rMatch)
              | otherwise = Nothing 
              where rMatch = find (isJust . (match w xs) . fst) [((drop n y), (take n y)) | n <- [1..(length y)]]

-- TESTS
-- substitute
testSubX = substitute 'x'
testSubstitute1 = (==) (testSubX "3*cos(x) + 4 - x" "5.37") "3*cos(5.37) + 4 - 5.37"
testSubstitute2 = (==) (testSubX "3*cos(x) + 4" "5.37") "3*cos(5.37) + 4"
testSubstitute3 = (==) (testSubX "3*cos(4) + 4" "5.37") "3*cos(4) + 4"
testSubstitute4 = (==) (testSubX "xxx" "Hello") "HelloHelloHello"
testSubstitute = and [testSubstitute1, testSubstitute2, testSubstitute3, testSubstitute4]

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
testMatch = and [testMatch1, testMatch2, testMatch3, testMatch4, testMatch5, testMatch6, testMatch7, testMatch8]

testPart2 = and [testSubstitute, testMatch]

