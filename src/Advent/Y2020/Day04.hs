{- Passport Processing -}
module Advent.Y2020.Day04 (part1, part2) where

import Advent.Util.Parsing (Parser, parseOrError)
import qualified Advent.Util.Parsing as P
import qualified Data.Char as C
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Text.Megaparsec as M
import qualified Text.Megaparsec.Char as M

{- Part 1

You arrive at the airport only to realize that you grabbed your North Pole
Credentials instead of your passport. While these documents are extremely
similar, North Pole Credentials aren't issued by a country and therefore aren't
actually valid documentation for travel in most of the world.

It seems like you're not the only one having problems, though; a very long line
has formed for the automatic passport scanners, and the delay could upset your
travel itinerary.

Due to some questionable network security, you realize you might be able to
solve both of these problems at the same time.

The automatic passport scanners are slow because they're having trouble
detecting which passports have all required fields. The expected fields are as
follows:

  - byr (Birth Year)
  - iyr (Issue Year)
  - eyr (Expiration Year)
  - hgt (Height)
  - hcl (Hair Color)
  - ecl (Eye Color)
  - pid (Passport ID)
  - cid (Country ID)

Passport data is validated in batch files (your puzzle input). Each passport is
represented as a sequence of key:value pairs separated by spaces or newlines.
Passports are separated by blank lines.

Here is an example batch file containing four passports:

  ecl:gry pid:860033327 eyr:2020 hcl:#fffffd
  byr:1937 iyr:2017 cid:147 hgt:183cm

  iyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884
  hcl:#cfa07d byr:1929

  hcl:#ae17e1 iyr:2013
  eyr:2024
  ecl:brn pid:760753108 byr:1931
  hgt:179cm

  hcl:#cfa07d eyr:2025 pid:166559648
  iyr:2011 ecl:brn hgt:59in

The first passport is valid - all eight fields are present. The second passport
is invalid - it is missing hgt (the Height field).

The third passport is interesting; the only missing field is cid, so it looks
like data from North Pole Credentials, not a passport at all! Surely, nobody
would mind if you made the system temporarily ignore missing cid fields. Treat
this "passport" as valid.

The fourth passport is missing two fields, cid and byr. Missing cid is fine,
but missing any other field is not, so this passport is invalid.

According to the above rules, your improved system would report 2 valid
passports.

Count the number of valid passports - those that have all required fields.
Treat cid as optional. In your batch file, how many passports are valid?

-}

type Passport = Map String String

pWord :: Parser String
pWord = M.takeWhileP (Just "word") (not . C.isSpace)

pPassport :: Parser Passport
pPassport = Map.fromList <$> pField `P.sepBy1NG` M.spaceChar
  where
    pField = do
      k <- pKey
      _ <- M.char ':'
      v <- pWord
      return (k, v)
    pKey = P.stringChoice [ "byr"
                          , "iyr"
                          , "eyr"
                          , "hgt"
                          , "hcl"
                          , "ecl"
                          , "pid"
                          , "cid"
                          ]

parseInput :: String -> [Passport]
parseInput = parseOrError $ pPassport `P.sepBy1NG` M.count 2 M.newline

isValid1 :: Passport -> Bool
isValid1 p = all (`Map.member` p) requiredKeys
  where requiredKeys = [ "byr"
                       , "iyr"
                       , "eyr"
                       , "hgt"
                       , "hcl"
                       , "ecl"
                       , "pid"
                       -- , "cid" -- ignore
                       ]

-- | >>> part1 "ecl:gry pid:860033327 eyr:2020 hcl:#fffffd\nbyr:1937 iyr:2017 cid:147 hgt:183cm\n\niyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884\nhcl:#cfa07d byr:1929\n\nhcl:#ae17e1 iyr:2013\neyr:2024\necl:brn pid:760753108 byr:1931\nhgt:179cm\n\nhcl:#cfa07d eyr:2025 pid:166559648\niyr:2011 ecl:brn hgt:59in\n"
-- 2
-- >>> part1 <$> readFile "input/2020/day04.txt"
-- 192
part1 :: String -> Int
part1 = length . filter isValid1 . parseInput


{- Part 2

The line is moving more quickly now, but you overhear airport security talking
about how passports with invalid data are getting through. Better add some data
validation, quick!

You can continue to ignore the cid field, but each other field has strict rules
about what values are valid for automatic validation:

  - byr (Birth Year) - four digits; at least 1920 and at most 2002.
  - iyr (Issue Year) - four digits; at least 2010 and at most 2020.
  - eyr (Expiration Year) - four digits; at least 2020 and at most 2030.
  - hgt (Height) - a number followed by either cm or in:
      - If cm, the number must be at least 150 and at most 193.
      - If in, the number must be at least 59 and at most 76.
  - hcl (Hair Color) - a # followed by exactly six characters 0-9 or a-f.
  - ecl (Eye Color) - exactly one of: amb blu brn gry grn hzl oth.
  - pid (Passport ID) - a nine-digit number, including leading zeroes.
  - cid (Country ID) - ignored, missing or not.

Your job is to count the passports where all required fields are both present
and valid according to the above rules. Here are some example values:

  byr valid:   2002
  byr invalid: 2003

  hgt valid:   60in
  hgt valid:   190cm
  hgt invalid: 190in
  hgt invalid: 190

  hcl valid:   #123abc
  hcl invalid: #123abz
  hcl invalid: 123abc

  ecl valid:   brn
  ecl invalid: wat

  pid valid:   000000001
  pid invalid: 0123456789

Here are some invalid passports:

  eyr:1972 cid:100
  hcl:#18171d ecl:amb hgt:170 pid:186cm iyr:2018 byr:1926

  iyr:2019
  hcl:#602927 eyr:1967 hgt:170cm
  ecl:grn pid:012533040 byr:1946

  hcl:dab227 iyr:2012
  ecl:brn hgt:182cm pid:021572410 eyr:2020 byr:1992 cid:277

  hgt:59cm ecl:zzz
  eyr:2038 hcl:74454a iyr:2023
  pid:3556412378 byr:2007

Here are some valid passports:

  pid:087499704 hgt:74in ecl:grn iyr:2012 eyr:2030 byr:1980
  hcl:#623a2f

  eyr:2029 ecl:blu cid:129 byr:1989
  iyr:2014 pid:896056539 hcl:#a97842 hgt:165cm

  hcl:#888785
  hgt:164cm byr:2001 iyr:2015 cid:88
  pid:545766238 ecl:hzl
  eyr:2022

  iyr:2010 hgt:158cm hcl:#b6652a ecl:blu byr:1944 eyr:2021 pid:093154719

Count the number of valid passports - those that have all required fields and
valid values. Continue to treat cid as optional. In your batch file, how many
passports are valid?

-}

data ParseResult = Valid Passport
                 | Invalid Passport
                 deriving (Show)

isValid2 :: ParseResult -> Bool
isValid2 (Valid   _) = True
isValid2 (Invalid _) = False

pValidPassport :: Parser ParseResult
pValidPassport =
  Valid <$> P.constrained
              isValid1
              "Must have all fields present"
              pPassportMap
  where
    pPassportMap = Map.fromList <$> pField `P.sepBy1NG` M.spaceChar
    pField = M.choice [ pPair "byr" byr
                      , pPair "iyr" iyr
                      , pPair "eyr" eyr
                      , pPair "hgt" hgt
                      , pPair "hcl" hcl
                      , pPair "ecl" ecl
                      , pPair "pid" pid
                      , pPair "cid" cid
                      ]
    byr = decParser 1920 2002
    iyr = decParser 2010 2020
    eyr = decParser 2020 2030
    hgt = M.try hgtCm M.<|> hgtIn
    hgtCm = (++) <$> decParser 150 193 <*> M.string "cm"
    hgtIn = (++) <$> decParser  59  76 <*> M.string "in"
    hcl = (++) <$> M.string "#" <*> M.count 6 M.hexDigitChar
    ecl = P.stringChoice ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]
    pid = M.count 9 M.digitChar
    cid = pWord

    pPair :: String -> Parser String -> Parser (String, String)
    pPair k pV = M.try $ (,) <$> M.string k <* M.char ':' <*> pV

    decParser :: Int -> Int -> Parser String
    decParser x y = show <$> P.decimalInRange x y

pInvalidPassport :: Parser ParseResult
pInvalidPassport = Invalid <$> pPassport

-- | >>> map isValid2 $ parseInput2 "eyr:1972 cid:100\nhcl:#18171d ecl:amb hgt:170 pid:186cm iyr:2018 byr:1926\n\niyr:2019\nhcl:#602927 eyr:1967 hgt:170cm\necl:grn pid:012533040 byr:1946\n\nhcl:dab227 iyr:2012\necl:brn hgt:182cm pid:021572410 eyr:2020 byr:1992 cid:277\n\nhgt:59cm ecl:zzz\neyr:2038 hcl:74454a iyr:2023\npid:3556412378 byr:2007\n"
-- [False, False, False, False]
-- >>> map isValid2 $ parseInput2 "pid:087499704 hgt:74in ecl:grn iyr:2012 eyr:2030 byr:1980\nhcl:#623a2f\n\neyr:2029 ecl:blu cid:129 byr:1989\niyr:2014 pid:896056539 hcl:#a97842 hgt:165cm\n\nhcl:#888785\nhgt:164cm byr:2001 iyr:2015 cid:88\npid:545766238 ecl:hzl\neyr:2022\n\niyr:2010 hgt:158cm hcl:#b6652a ecl:blu byr:1944 eyr:2021 pid:093154719\n"
-- [True, True, True, True]
parseInput2 :: String -> [ParseResult]
parseInput2 = parseOrError $ pAnyPassport `P.sepBy1NG` M.count 2 M.newline
  where pAnyPassport = M.try pValidPassport M.<|> pInvalidPassport

-- | >>> part2 <$> readFile "input/2020/day04.txt"
-- 101
part2 :: String -> Int
part2 = length . filter isValid2 . parseInput2
