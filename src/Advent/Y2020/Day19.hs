{- Monster Messages -}
module Advent.Y2020.Day19 (part1, part2) where

import Advent.Util.Parsing (Parser, parseOrError)
import qualified Advent.Util.Parsing as P
import Data.List (intercalate, sort)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (mapMaybe)
import qualified Text.Megaparsec as M
import qualified Text.Megaparsec.Char as M
import qualified Text.Megaparsec.Char.Lexer as L

{- Part 1

You land in an airport surrounded by dense forest. As you walk to your
high-speed train, the Elves at the Mythical Information Bureau contact you
again. They think their satellite has collected an image of a sea monster!
Unfortunately, the connection to the satellite is having problems, and many of
the messages sent back from the satellite have been corrupted.

They sent you a list of the rules valid messages should obey and a list of
received messages they've collected so far (your puzzle input).

The rules for valid messages (the top part of your puzzle input) are numbered
and build upon each other. For example:

    0: 1 2
    1: "a"
    2: 1 3 | 3 1
    3: "b"

Some rules, like 3: "b", simply match a single character (in this case, b).

The remaining rules list the sub-rules that must be followed; for example, the
rule 0: 1 2 means that to match rule 0, the text being checked must match rule
1, and the text after the part that matched rule 1 must then match rule 2.

Some of the rules have multiple lists of sub-rules separated by a pipe (|).
This means that at least one list of sub-rules must match. (The ones that match
might be different each time the rule is encountered.) For example, the rule 2:
1 3 | 3 1 means that to match rule 2, the text being checked must match rule 1
followed by rule 3 or it must match rule 3 followed by rule 1.

Fortunately, there are no loops in the rules, so the list of possible matches
will be finite. Since rule 1 matches a and rule 3 matches b, rule 2 matches
either ab or ba. Therefore, rule 0 matches aab or aba.

Here's a more interesting example:

    0: 4 1 5
    1: 2 3 | 3 2
    2: 4 4 | 5 5
    3: 4 5 | 5 4
    4: "a"
    5: "b"

Here, because rule 4 matches a and rule 5 matches b, rule 2 matches two letters
that are the same (aa or bb), and rule 3 matches two letters that are different
(ab or ba).

Since rule 1 matches rules 2 and 3 once each in either order, it must match two
pairs of letters, one pair with matching letters and one pair with different
letters. This leaves eight possibilities: aaab, aaba, bbab, bbba, abaa, abbb,
baaa, or babb.

Rule 0, therefore, matches a (rule 4), then any of the eight options from rule
1, then b (rule 5): aaaabb, aaabab, abbabb, abbbab, aabaab, aabbbb, abaaab, or
ababbb.

The received messages (the bottom part of your puzzle input) need to be checked
against the rules so you can determine which are valid and which are corrupted.
Including the rules and the messages together, this might look like:

    0: 4 1 5
    1: 2 3 | 3 2
    2: 4 4 | 5 5
    3: 4 5 | 5 4
    4: "a"
    5: "b"

    ababbb
    bababa
    abbbab
    aaabbb
    aaaabbb

Your goal is to determine the number of messages that completely match rule 0.
In the above example, ababbb and abbbab match, but bababa, aaabbb, and aaaabbb
do not, producing the answer 2. The whole message must match all of rule 0;
there can't be extra unmatched characters in the message. (For example, aaaabbb
might appear to match rule 0 above, but it has an extra unmatched b on the
end.)

How many messages completely match rule 0?

-}


-- Input data types --

type Message = String

data Rule = StringRule String
          | LookupRule Int
          | ListRule [Rule]
          | ChoiceRule [Rule]

type RuleMap = Map Int Rule

instance Show Rule where
  show (StringRule s) = show s
  show (LookupRule x) = show x
  show (ListRule xs) = unwords $ map show xs
  show (ChoiceRule xs) = "(" ++ intercalate " | " (map show xs) ++ ")"

_showRules :: RuleMap -> String
_showRules rules =
  intercalate "\n" [ show k ++ ": " ++ show (rules Map.! k)
                   | k <- sort (Map.keys rules) ]


-- Parsing input into Rules and Messages --

pRule :: Parser Rule
pRule = pChoiceOrSimple
  where pString :: Parser Rule
        pString = do
          _ <- M.char '"'
          s <- M.takeWhileP (Just "String") (/= '"')
          _ <- M.char '"'
          return $ StringRule s

        pLookup :: Parser Rule
        pLookup = LookupRule <$> L.decimal

        pSimple = M.choice [ pString, pLookup ]

        pListOrSimple :: Parser Rule
        pListOrSimple = do
          s <- pSimple `P.sepByNG` M.char ' '
          return $ case s of
                     [x] -> x
                     xs -> ListRule xs

        pChoiceOrSimple :: Parser Rule
        pChoiceOrSimple = do
          l <- pListOrSimple `P.sepByNG` M.string " | "
          return $ case l of
                     [x] -> x
                     xs -> ChoiceRule xs

pRuleLine :: Parser (Int, Rule)
pRuleLine = do
  num <- L.decimal
  _ <- M.string ": "
  rule <- pRule
  return (num, rule)

pRules :: Parser RuleMap
pRules = Map.fromList <$> pRuleLine `M.sepEndBy` M.newline

parseInput :: String -> (RuleMap, [Message])
parseInput = parseOrError pInput
  where
    pInput = do
      rules <- pRules
      _ <- M.some M.newline
      messages <- M.takeWhile1P (Just "line") (/= '\n') `M.sepEndBy` M.newline
      return (rules, messages)


-- Parsing a Message using a RuleMap --

parserFromRule :: RuleMap -> Rule -> Parser Message
parserFromRule _ (StringRule s)  = M.string s
parserFromRule m (LookupRule x)  = parserFromRule m (m Map.! x)
parserFromRule m (ListRule xs)   = concat <$> mapM (parserFromRule m) xs
parserFromRule m (ChoiceRule xs) = M.choice . map (M.try . parserFromRule m) $ xs

ruleParserAt :: RuleMap -> Int -> Parser Message
ruleParserAt m n = parserFromRule m (LookupRule n)

-- $setup
-- >>> let rule1 = parseOrError pRules "0: 1 2\n1: \"a\"\n2: 1 3 | 3 1\n3: \"b\""

-- | Parses a message starting from Rule 0
-- >>> parseWithRules1 rule1 "aba"
-- Just "aba"
-- >>> parseWithRules1 rule1 "aab"
-- Just "aab"
-- >>> parseWithRules1 rule1 "aaa"
-- Nothing
parseWithRules1 :: RuleMap -> String -> Maybe Message
parseWithRules1 m = M.parseMaybe (ruleParserAt m 0)


-- | >>> part1 "0: 4 1 5\n1: 2 3 | 3 2\n2: 4 4 | 5 5\n3: 4 5 | 5 4\n4: \"a\"\n5: \"b\"\n\nababbb\nbababa\nabbbab\naaabbb\naaaabbb"
-- 2
-- >>> part1 "42: 9 14 | 10 1\n9: 14 27 | 1 26\n10: 23 14 | 28 1\n1: \"a\"\n11: 42 31\n5: 1 14 | 15 1\n19: 14 1 | 14 14\n12: 24 14 | 19 1\n16: 15 1 | 14 14\n31: 14 17 | 1 13\n6: 14 14 | 1 14\n2: 1 24 | 14 4\n0: 8 11\n13: 14 3 | 1 12\n15: 1 | 14\n17: 14 2 | 1 7\n23: 25 1 | 22 14\n28: 16 1\n4: 1 1\n20: 14 14 | 1 15\n3: 5 14 | 16 1\n27: 1 6 | 14 18\n14: \"b\"\n21: 14 1 | 1 14\n25: 1 1 | 1 14\n22: 14 14\n8: 42\n26: 14 22 | 1 20\n18: 15 15\n7: 14 5 | 1 21\n24: 14 1\n\nabbbbbabbbaaaababbaabbbbabababbbabbbbbbabaaaa\nbbabbbbaabaabba\nbabbbbaabbbbbabbbbbbaabaaabaaa\naaabbbbbbaaaabaababaabababbabaaabbababababaaa\nbbbbbbbaaaabbbbaaabbabaaa\nbbbababbbbaaaaaaaabbababaaababaabab\nababaaaaaabaaab\nababaaaaabbbaba\nbaabbaaaabbaaaababbaababb\nabbbbabbbbaaaababbbbbbaaaababb\naaaaabbaabaaaaababaa\naaaabbaaaabbaaa\naaaabbaabbaaaaaaabbbabbbaaabbaabaaa\nbabaaabbbaaabaababbaabababaaab\naabbbbbaabbbaaaaaabbbbbababaaaaabbaaabba"
-- 3
-- >>> part1 <$> readFile "input/2020/day19.txt"
-- 132
part1 :: String -> Int
part1 input =
  let (rules, messages) = parseInput input
  in length . mapMaybe (parseWithRules1 rules) $ messages


{- Part 2

As you look over the list of messages, you realize your matching rules aren't
quite right. To fix them, completely replace rules 8: 42 and 11: 42 31 with the
following:

    8: 42 | 42 8
    11: 42 31 | 42 11 31

This small change has a big impact: now, the rules do contain loops, and the
list of messages they could hypothetically match is infinite. You'll need to
determine how these changes affect which messages are valid.

Fortunately, many of the rules are unaffected by this change; it might help to
start by looking at which rules always match the same set of values and how
those rules (especially rules 42 and 31) are used by the new versions of rules
8 and 11.

(Remember, you only need to handle the rules you have; building a solution that
could handle any hypothetical combination of rules would be significantly more
difficult.)

For example:

    42: 9 14 | 10 1
    9: 14 27 | 1 26
    10: 23 14 | 28 1
    1: "a"
    11: 42 31
    5: 1 14 | 15 1
    19: 14 1 | 14 14
    12: 24 14 | 19 1
    16: 15 1 | 14 14
    31: 14 17 | 1 13
    6: 14 14 | 1 14
    2: 1 24 | 14 4
    0: 8 11
    13: 14 3 | 1 12
    15: 1 | 14
    17: 14 2 | 1 7
    23: 25 1 | 22 14
    28: 16 1
    4: 1 1
    20: 14 14 | 1 15
    3: 5 14 | 16 1
    27: 1 6 | 14 18
    14: "b"
    21: 14 1 | 1 14
    25: 1 1 | 1 14
    22: 14 14
    8: 42
    26: 14 22 | 1 20
    18: 15 15
    7: 14 5 | 1 21
    24: 14 1

    abbbbbabbbaaaababbaabbbbabababbbabbbbbbabaaaa
    bbabbbbaabaabba
    babbbbaabbbbbabbbbbbaabaaabaaa
    aaabbbbbbaaaabaababaabababbabaaabbababababaaa
    bbbbbbbaaaabbbbaaabbabaaa
    bbbababbbbaaaaaaaabbababaaababaabab
    ababaaaaaabaaab
    ababaaaaabbbaba
    baabbaaaabbaaaababbaababb
    abbbbabbbbaaaababbbbbbaaaababb
    aaaaabbaabaaaaababaa
    aaaabbaaaabbaaa
    aaaabbaabbaaaaaaabbbabbbaaabbaabaaa
    babaaabbbaaabaababbaabababaaab
    aabbbbbaabbbaaaaaabbbbbababaaaaabbaaabba

Without updating rules 8 and 11, these rules only match three messages:
bbabbbbaabaabba, ababaaaaaabaaab, and ababaaaaabbbaba.

However, after updating rules 8 and 11, a total of 12 messages match:

  - bbabbbbaabaabba
  - babbbbaabbbbbabbbbbbaabaaabaaa
  - aaabbbbbbaaaabaababaabababbabaaabbababababaaa
  - bbbbbbbaaaabbbbaaabbabaaa
  - bbbababbbbaaaaaaaabbababaaababaabab
  - ababaaaaaabaaab
  - ababaaaaabbbaba
  - baabbaaaabbaaaababbaababb
  - abbbbabbbbaaaababbbbbbaaaababb
  - aaaaabbaabaaaaababaa
  - aaaabbaabbaaaaaaabbbabbbaaabbaabaaa
  - aabbbbbaabbbaaaaaabbbbbababaaaaabbaaabba

After updating rules 8 and 11, how many messages completely match rule 0?

-}

-- | Parses a message assuming the following rules are in place:
--
--    0: 8 11
--    8: 42 | 42 8
--    11: 42 31 | 42 11 31
--
-- Or to simplify a little, "(rule 42)+ (rule 31)+" but where rule 42 must
-- match more times than rule 31.
-- NB: the 8 and 11 rules are specified in the instructions, and both the
-- sample input and the real input use "0: 8 11"
parseWithRules2 :: RuleMap -> String -> Maybe Message
parseWithRules2 m =
  let rule42 = ruleParserAt m 42
      rule31 = ruleParserAt m 31
      parser :: Parser Message
      parser = do
        a <- M.some rule42
        b <- M.some rule31
        if length a > length b
           then return $ concat a ++ concat b
           else fail "Nope!"
  in M.parseMaybe parser

-- | >>> part2 "42: 9 14 | 10 1\n9: 14 27 | 1 26\n10: 23 14 | 28 1\n1: \"a\"\n11: 42 31\n5: 1 14 | 15 1\n19: 14 1 | 14 14\n12: 24 14 | 19 1\n16: 15 1 | 14 14\n31: 14 17 | 1 13\n6: 14 14 | 1 14\n2: 1 24 | 14 4\n0: 8 11\n13: 14 3 | 1 12\n15: 1 | 14\n17: 14 2 | 1 7\n23: 25 1 | 22 14\n28: 16 1\n4: 1 1\n20: 14 14 | 1 15\n3: 5 14 | 16 1\n27: 1 6 | 14 18\n14: \"b\"\n21: 14 1 | 1 14\n25: 1 1 | 1 14\n22: 14 14\n8: 42\n26: 14 22 | 1 20\n18: 15 15\n7: 14 5 | 1 21\n24: 14 1\n\nabbbbbabbbaaaababbaabbbbabababbbabbbbbbabaaaa\nbbabbbbaabaabba\nbabbbbaabbbbbabbbbbbaabaaabaaa\naaabbbbbbaaaabaababaabababbabaaabbababababaaa\nbbbbbbbaaaabbbbaaabbabaaa\nbbbababbbbaaaaaaaabbababaaababaabab\nababaaaaaabaaab\nababaaaaabbbaba\nbaabbaaaabbaaaababbaababb\nabbbbabbbbaaaababbbbbbaaaababb\naaaaabbaabaaaaababaa\naaaabbaaaabbaaa\naaaabbaabbaaaaaaabbbabbbaaabbaabaaa\nbabaaabbbaaabaababbaabababaaab\naabbbbbaabbbaaaaaabbbbbababaaaaabbaaabba"
-- 12
-- >>> part2 <$> readFile "input/2020/day19.txt"
-- 306
part2 :: String -> Int
part2 input =
  let (rules, messages) = parseInput input
  in length . mapMaybe (parseWithRules2 rules) $ messages
