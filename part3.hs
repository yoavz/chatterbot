module Part3
( transformationApply
, transformationsApply
) where 

import Useful
import Part2
import Data.Maybe

transformationApply :: Eq a => a -> ([a] -> [a]) -> 
                       [a] -> ([a], [a]) -> Maybe [a]
transformationApply w f list pattern 
    | isJust matchRes = Just (substitute w pattern2 (f (fromJust matchRes)))
    | otherwise = Nothing
    where pattern1 = (fst pattern) 
          pattern2 = (snd pattern)
          matchRes = match w pattern1 list

transformationsApply :: Eq a => a -> ([a] -> [a]) -> 
                        [([a], [a])] -> [a] -> Maybe [a]
transformationsApply _ _ [] _ = Nothing
transformationsApply w f (p:ps) list =
    orElse (transformationApply w f list p) (transformationsApply w f ps list)

-- TESTS
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
