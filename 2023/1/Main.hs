
module Main where

import Control.Applicative
import Data.Char
import Data.List

import Text.Yoda

import Debug.Trace

main :: IO ()
main = do
  f <- readFile "input.txt"
  putStrLn "Part 1:"
  print (process f)
  putStrLn "Part 2:"
  print (process'' f)

-- part 1
process :: String -> Int
process = sum
 . map ( read
       . onlyFirstAndLastOrDouble
       . filter isDigit)
 . lines

onlyFirstAndLastOrDouble :: [a] -> [a]
onlyFirstAndLastOrDouble l
  | length l == 1 = l ++ l
  | otherwise = head l : [last l]

theirExample = "1abc2\npqr3stu8vwx\na1b2c3d4e5f\ntreb7uchet"

-- >>> process theirExample
-- 77

example = "heightseven4two5\nnpskfdstpk2knsm\ndjnrmpxjbsbpgzvtjkhq6pkkfshx"

-- >>> process example
-- 52

-- part 2
process' :: String -> Int
process' = sum
 . map ( turnFirstAndLastIntoNumber
       . takeBestParse
       . parse parseDigits)
 . lines

theirExample' = "two1nine\neightwothree\nabcone2threexyz\nxtwone3four\n4nineeightseven2\nzoneight234\n7pqrstsixteen"

-- >>> process' theirExample'
-- 281

turnFirstAndLastIntoNumber :: [Int] -> Int
turnFirstAndLastIntoNumber xs
 = let h = head xs
       l = last xs
    in read $ show h ++ show l

-- both types

gobble :: Parser [Char]
gobble = many item

parseDigits :: Parser [Int]
parseDigits
  = many ( wordyDigit
        <|> digit
        <|> gobble *> wordyDigit <* gobble
        <|> gobble *> digit <* gobble)

digit :: Parser Int
digit = digitToInt <$> satisfy isDigit

notDigit :: Parser Char
notDigit = satisfy (not . isDigit)

one = string "one"
two = string "two"
three = string "three"
four = string "four"
five = string "five"
six = string "six"
seven = string "seven"
eight = string "eight"
nine = string "nine"
twone = string "twone"

match :: String -> Int
match "one"   = 1
match "two"   = 2
match "three" = 3
match "four"  = 4
match "five"  = 5
match "six"   = 6
match "seven" = 7
match "eight" = 8
match "nine"  = 9

wordyDigit :: Parser Int
wordyDigit = match <$> choice [one, two, three, four, five, six, seven, eight, nine]

takeBestParse :: Ord a => [([a], String)] -> [a]
takeBestParse xs
 = fst $ maximumBy (\(x,_) (y,_) -> compare (length x) (length y)) xs

-- parsing not right solution, cos its unstructured
-- what you should do is process each line, by picking out all the numbers (regadless of overlap and in order)
-- then pick the first and last

-- not parsing

process'' :: String -> Int
process'' = sum
 . map ( turnFirstAndLastIntoNumber
       . processLine)
 . lines

processLine :: String -> [Int]
processLine [] = []
processLine (x:xs) | isDigit x = digitToInt x : processLine xs
processLine ('o':'n':'e':xs) = 1 : processLine ('e':xs)
processLine ('t':'w':'o':xs) = 2 : processLine ('o':xs)
processLine ('t':'h':'r':'e':'e':xs) = 3 : processLine ('e':xs)
processLine ('f':'o':'u':'r':xs) = 4 : processLine xs
processLine ('f':'i':'v':'e':xs) = 5 : processLine ('e':xs)
processLine ('s':'i':'x':xs) = 6 : processLine xs
processLine ('s':'e':'v':'e':'n':xs) = 7 : processLine ('n':xs)
processLine ('e':'i':'g':'h':'t':xs) = 8 : processLine ('t':xs)
processLine ('n':'i':'n':'e':xs) = 9 : processLine ('e':xs)
processLine (x:xs) = processLine xs

-- NOT 54538