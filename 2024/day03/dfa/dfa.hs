{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-|

Run this with runHaskell.

-}
module Main where

import           Data.Char   (isDigit)
import           Data.List   (foldl', isPrefixOf, stripPrefix)
import           Data.Maybe  (fromJust)
import           Data.Monoid (Sum(..))
import qualified Data.Set    as Set

{-
  You can skip till 'main'


  × looks more like math, thats why we use it here
-}

type a × b = a -> b
infixr 0 ×

-- otherwise × parses a -> b -> c as (a -> b) -> c
type a → b = a -> b
infixr 0 →

-- program starts here

-- our Alphabet
-- technically only one of :n>^[%760@]&4$l5ym o!\n<#?,~p)}srah/(e2{1u+-8'w39*tcid;fb
type Σ = Char
type Σ' = String -- Σ*

data Q
  = Q0 -- ^ do() (initial)
  | Q1 -- ^ don't()
  | Q2 -- ^ mult(
  | Q3 -- ^ [1-9] (after 'mult(') ) (NOTE: this is technically not correkt only {1,3} chars allowed)
  | Q4 -- ^ [0-9] (after 'Q3')
  -- | [1--9] (after 'Q4')
  | Q5
  -- for Set
  deriving (Eq, Ord, Show)

-- initial state
q0 :: Q
q0 = Q0

-- Final states (not relevant for us)
_F :: Set.Set Q
_F = Set.fromList [ Q0 , Q1  ]

{- |
  With the current implementation this is not possible, we would need to decide
  if we need to go to stay or go depending on whether we get do() or d<something> else.
  Wich we cant do with just one char.


  We will define a function δ' that takes a string as input instead
  (similar to δ* for dfa's).

  transition function δ
-}
δ :: Q × Σ → Q
δ q c = undefined {- impossible ! -}

{-
  There is just one problem:
  We need to return what we read or the new string,
  otherwise we will not be able to continue later on


  We have 2 options here:
    1. Implement this over the State Monad and keep all the knowledge hidden
    2. Return a tuple Q × Σ' where Σ' is what we just read / the new string (both options are possible).
-}
δ' :: Q × Σ' → Q
δ' q str@(c:rest) = case (q, c) of
  (Q0, _) | "mul("    `isPrefixOf` str -> Q2 -- try to read mul
  (Q0, _) | "don't()" `isPrefixOf` str -> Q1 -- dont read anymore
  (Q1, _) | "do()"    `isPrefixOf` str -> Q0 -- read again
  (Q0, c)                              -> Q0
  (Q1, c)                              -> Q1
  (Q2, c) | isDigit c && c /= '0'      -> Q3 -- here we lose the knowledge about c
  (Q2, _)                              -> error "Not valid" -- We should really go back to Q0
  (Q3, ',')                            -> Q4 -- num finished, next num (we lose all knowledge about the read char though)
  (Q3, c) | isDigit c                  -> Q3 -- we stay, but lose information here
  (Q3, _)                              -> error "Not in input" -- We should really go back to Q0
  (Q4, c) | isDigit c && c /= '0'      -> Q5 -- see above Q2 -> Q3
  (Q4, _)                              -> error "Not valid" -- We should really go back to Q0
  (Q5, c) | isDigit c                  -> Q5 -- we stay, but lose information
  (Q5, ')')                            -> Q0 -- losing information (about both of our numbers)

{-
  Before we define the new transition function we define
  a new alphabet which we will return

  We will use a typealias (type) for convienience sake.

  Maybe is either a Nothing or a Just value.
  Just like Option in many other languges.
-}

type R = (Σ', Maybe Σ) -- We might return some char we read (if it is interesting)

{-
  To combat the issues above we define δ''

  The second element in the return tuple will be the match we had
-}
δ'' :: Q × Σ' → (Q, R)
δ'' q str@(c:rest) = case (q,c) of
  (Q0, _) | "mul("    `isPrefixOf` str -> (Q2, withoutPrefix "mul(") -- try to read mul
  (Q0, _) | "don't()" `isPrefixOf` str -> (Q1, withoutPrefix "don't()") -- dont read anymore
  (Q1, _) | "do()"    `isPrefixOf` str -> (Q0, withoutPrefix "do()") -- read again
  (Q0, c)                              -> (Q0, nothing)
  (Q1, c)                              -> (Q1, nothing)
  (Q2, c) | isDigit c && c /= '0'      -> (Q3, just c) -- push the newly read digit along
  (Q2, _)                              -> (Q0, nothing)
  (Q3, ',')                            -> (Q4, nothing)
  (Q3, c) | isDigit c                  -> (Q3, just c) -- we stay
  (Q3, _)                              -> (Q0, nothing)
  (Q4, c) | isDigit c && c /= '0'      -> (Q5, just c) -- push the newly read digit along
  (Q4, _)                              -> (Q0, nothing)
  (Q5, c) | isDigit c                  -> (Q5, just c) -- we stay
  (Q5, ')')                            -> (Q0, just c) -- we need to indicate that we did not meet ')'
  (Q5, c)                              -> (Q0, nothing)
  where
    withoutPrefix pref = (fromJust $ pref `stripPrefix` str, Nothing)
    just c = (rest, Just c)
    nothing = (rest, Nothing)

testString :: Σ'
testString = "sldfjsfdmul(12,343)sldfjsddon't()sldfjmul(234,3)fsldjdo()sd081234234nsdfjlsj012mul(4332,34lnäaKSD#A+WER09834fmul(2,3)"
--                    ^^^^^^^^^^^       ^^^^^^^     ^^^^^^^^^      ^^^^                      !!!!!!!!!!!                   ^^^^^^^^

{-
  How would we now use this function?
-}
test :: Σ' -> [(Q, Maybe Σ)]
test s =
  let loop q r = case r of
            []  -> []
            str -> let t@(q', (str', c)) = δ'' q str in (q', c) : loop q' str'
  in loop q0 s

{-
  If we run this now, we get back a list
  of R's.

  Which means tuples of new states and returned Chars
-}

-- >>> test testString
-- [(Q0,Nothing),(Q0,Nothing),(Q0,Nothing),
--  (Q0,Nothing),(Q0,Nothing),(Q0,Nothing),
--  (Q0,Nothing),(Q0,Nothing),(Q2,Nothing),
--  (Q3,Just '1'),(Q3,Just '2'),(Q4,Nothing),
--  (Q5,Just '3'),(Q5,Just '4'),(Q5,Just '3'),
--  (Q0,Just ')'),(Q0,Nothing),
--  (Q0,Nothing),(Q0,Nothing),(Q0,Nothing),
--  (Q0,Nothing),(Q0,Nothing),(Q0,Nothing),
--  (Q1,Nothing),(Q1,Nothing),(Q1,Nothing),
--  (Q1,Nothing),(Q1,Nothing),(Q1,Nothing),
--  (Q1,Nothing),(Q1,Nothing),(Q1,Nothing),
--  (Q1,Nothing),(Q1,Nothing),(Q1,Nothing),
--  (Q1,Nothing),(Q1,Nothing),(Q1,Nothing),
--  (Q1,Nothing),(Q1,Nothing),(Q1,Nothing),
--  (Q1,Nothing),(Q1,Nothing),(Q1,Nothing),
--  (Q0,Nothing),(Q0,Nothing),(Q0,Nothing),
--  (Q0,Nothing),(Q0,Nothing),(Q0,Nothing),
--  (Q0,Nothing),(Q0,Nothing),(Q0,Nothing),
--  (Q0,Nothing),(Q0,Nothing),(Q0,Nothing),
--  (Q0,Nothing),(Q0,Nothing),(Q0,Nothing),
--  (Q0,Nothing),(Q0,Nothing),(Q0,Nothing),
--  (Q0,Nothing),(Q0,Nothing),(Q0,Nothing),
--  (Q0,Nothing),(Q0,Nothing),(Q2,Nothing),
--  (Q3,Just '4'),(Q3,Just '3'),(Q3,Just '3'),
--  (Q3,Just '2'),(Q4,Nothing),(Q5,Just '3'),
--
--  (Q5,Just '4'),(Q0,Nothing),(Q0,Nothing), <-- see here we're not getting a Just '(', because it was followed by a 'l' and is not valid
--
--  (Q0,Nothing),(Q0,Nothing),(Q0,Nothing),
--  (Q0,Nothing),(Q0,Nothing),(Q0,Nothing),
--  (Q0,Nothing),(Q0,Nothing),(Q0,Nothing),
--  (Q0,Nothing),(Q0,Nothing),(Q0,Nothing),
--  (Q0,Nothing),(Q0,Nothing),(Q0,Nothing),
--  (Q0,Nothing),(Q0,Nothing),(Q2,Nothing),
--  (Q3,Just '2'),(Q4,Nothing),(Q5,Just '3'),
--  (Q0,Just ')')]


{-
  We could now go through this list and parse the numbers.

  But what we're gonna do instead is ignore all the Nothing values
  right away and introduce a 'Memory' and use that 'Memory'
  to keep track of the final sum and the values.

  We need such a memory, because Finite Auomata can only remember their last step,
  this is also why a set like { a^{n}b^{n} | n in N } is not a regular language.
  There is no way we can get as many b's as a's, since we cannot remember the n.

-}

data Memory
  = Memory
      { _sum  :: Sum Int
        -- ^ Sum is a monoid over +, you can think of sum as just an Integer
      , q3num :: Σ'
        -- ^ will be empty if we're not in q3 to q5
      , q5num :: Σ'
        -- ^ will be empty if we're not in q5
      }
  deriving (Show)

initialMemory :: Memory
initialMemory = Memory {_sum=0, q3num=[], q5num=[]}

{-
  Were gonna define a helper for us that
  will parse q3num and q5num and put their product in the _sum field
-}

addNumbers :: Memory -> Memory
addNumbers memory =
  let q3num' = read @Int memory.q3num
      q5num' = read @Int memory.q5num
  in memory { q3num = [], q5num = [], _sum = memory._sum <> Sum (q3num' * q5num') }

{-
  We can now define the last function to properly go through the
  input string and aquire the final result using the Memory we defined above
-}

solve :: Q × Memory × Σ' → Memory
solve q memory []    = memory -- base case for the recursion
solve q memory input =
  let (q', (rest, char)) = δ'' q input in
  let newMemory = case q' of
        Q3 | Just c <- char -> memory { q3num = memory.q3num <> [c] } -- put char in q3num
        Q5 | Just c <- char -> memory { q5num = memory.q5num <> [c] } -- put char in q5num
        Q0 | Just c <- char ->
          if not (null memory.q3num) && not (null memory.q5num)
            then addNumbers memory
            else memory { q3num = [], q5num = [] }
        Q0 | Nothing <- char -> memory { q3num = [], q5num = []}
        _ -> memory
  in solve q' newMemory rest

test' :: Sum Int
test' = let res = solve q0 initialMemory testString
  in res._sum

-- >>> test'
-- Sum {getSum = 4122}

{- |
  The final solution.
-}
main :: IO ()
main = do
  content <- readFile "../../input/day03.txt"
  let res = solve q0 initialMemory content
  print res._sum

{-
  We defined a [Mealy Machine](https://en.wikipedia.org/wiki/Mealy_machine)

  where our transiton is δ''
            start state is q0
            input alphabet is Σ = Char
            output alphabet is R = (String, Maybe Char)

  Because we also needed to remember some information along the way
  we defined 'Memory'.

  There are othehr ways to achive the same result using the same techinques:
    - We could -- for example -- not use the Memory and fold / reduce over the List
      created by the 'test' function defined above
    - We could have used the 'Control.Monad.State' Monad to handle
      all the stateful computations more gracefully
    - We could have defined δ'' in terms of Q × Σ -> (Q, R), by
      introducing more states (here d, and o would have their own state and o --> q0 if its not followed by n for example)
      but this would uneccessarily complicate the δ'' function
-}

