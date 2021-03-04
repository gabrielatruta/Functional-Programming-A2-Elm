module Regex exposing (..)

type RegexPattern
  = Literal Char
  | Many RegexPattern
  | OneOf RegexPattern RegexPattern
  | Seq RegexPattern RegexPattern

{-
  The `Ok` variant represents the matched input and the rest of the unmatched input
  The `Err` variant represents the original input
-}
type alias RegexResult = Result (List Char) (List Char, List Char)

{-
  Returns the `Ok` variant if the literal character matches the first character of the string.
  If the string is empty or the characters don't match the `Err` variant should be returned.
  ```elm
  matchLit 'a' ['a', 'b', 'b'] == Ok (['a'], ['b', 'b'])
  matchLit 'c' ['a', 'b', 'b'] == Err ['a', 'b', 'b']
  matchLit 'c' [] == Err []
  ```
-}
matchLit: Char -> List Char -> RegexResult
matchLit ch str =
    case str of
        [] -> Err []
        x::xs -> if ch == x
                 then Ok ([x], xs)
                 else Err str

{-
  Matches `pat1` and then `pat2`. Returns `Ok` only if both succeed.
  ```elm
  matchSeq (Literal 'a') (Literal 'b') ['a', 'b', 'c'] == Ok (['a', 'b'], ['c'])
  matchSeq (Literal 'a') (Literal 'b') ['a', 'x', 'c'] == Err (['a', 'x', 'c'])
  matchSeq (Seq (Literal 'a') (Literal 'b')) (Literal 'c') ['a', 'b', 'c', 'd'] == Ok (['a', 'b', 'c'], ['d'])
  ```
-}


--Function that turns a RegexPattern into a list of characters
regexDeconstruct: RegexPattern -> List Char
regexDeconstruct pat =
    case pat of
        (Literal ch) -> [ch]
        (Many _) -> []
        (OneOf _ _) -> []
        (Seq patt1 patt2) -> List.append (regexDeconstruct patt1) (regexDeconstruct patt2)

--Function that returns the k-th character from a list of characters
indexChar: Int -> List Char -> Char
indexChar k sequence =
    let
        iterationChar: Int -> List Char -> Int -> Char
        iterationChar index seq q =
            case seq of
                [] -> '*'
                x::xs -> if (index == q) then x
                                    else iterationChar index xs (q + 1)
    in
        iterationChar k sequence 1

matchSeq : RegexPattern -> RegexPattern -> List Char -> RegexResult
matchSeq pat1 pat2 input =
    let
       fullSequence = List.append (regexDeconstruct pat1) (regexDeconstruct pat2) --makes a list of characters from both patterns
       len = List.length fullSequence
       matchSequenceHelper i length series =
           case series of
               [] -> Err input --if the input is smaller than the length of the pattern, we return Err
               x::xs -> if (length == 0) then Ok (fullSequence, List.drop len input) else --if we haven't encountered any Err, then we return Ok
                    case (matchLit x [indexChar i fullSequence]) of -- verifies if the i-th character from the pattern matches with the character at index i from input
                        (Ok _) -> matchSequenceHelper (i + 1) (length - 1) xs --we keep testing the next characters from pattern with the characters that remained in int
                        (Err _) -> Err input --if one of the char from pattern didn't match, we return Err
    in
        matchSequenceHelper 1 len input

{-
  Matches the pattern `pattern` zero or many times. Always returns the `Ok` variant.
  ```elm
  matchMany (Literal 'a') ['a', 'a', 'a'] == Ok (['a', 'a', 'a'], [])
  matchMany (Literal 'b') ['a', 'a', 'a'] == Ok ([], ['a', 'a', 'a'])
  matchMany (Literal 'b') ['b', 'a', 'b'] == Ok (['b'], ['a', 'b'])
  matchMany (Seq (Literal 'b') (Literal 'a')) ['b', 'a', 'b', 'a', 'c'] == Ok (['b', 'a', 'b', 'a'], ['c'])
  matchMany (Seq (Literal 'b') (Literal 'a')) ['b', 'a', 'c', 'a', 'c'] == Ok (['b', 'a'], ['c', 'a', 'c'])
  ```
-}
{-
i -> how many characters were matched
l -> the length of the list of chars we use to search for the pattern
Takes the characters from the pattern and tries to find a match. If a match is found, then it will continue the search
by making a recursive call on the remaining list of chars (the input list without those characters that were matched).
If there is no match, then it will return the list with all the matched character from input and another one with
the unmatched characters. If it has reached the end of the input or if the size of the list of chars we use to search
for the pattern is smaller than the size of the pattern, it returns the all the matched characters and unmatched ones
-}
matchMany : RegexPattern -> List Char -> RegexResult
matchMany pat input =
    let
        size = List.length (regexDeconstruct pat) --size of the pattern
        matchManyHelper i l series matched = if (l == 0 || l < size) then Ok (matched, List.drop i input)
            else
                case (match pat series) of
                    (Ok _) -> matchManyHelper (i + size) (l - size) (List.drop (i + size) input) (List.append matched (List.take size series))
                    (Err _) -> Ok (matched, series)
    in
        matchManyHelper 0 (List.length input) input []

{-
  Tries to match one of `pat1` and `pat2`, in this order. If `pat1` matches, its result is returned, else
  `pat2` is tried.
  ```elm
  matchOneOf (Literal 'a') (Literal 'b') ['a', 'a', 'a'] == Ok (['a'], ['a', 'a'])
  matchOneOf (Literal 'b') (Literal 'a') ['a', 'a', 'a'] == Ok (['a'], ['a', 'a'])
  matchOneOf (Seq (Literal 'a') (Literal 'b')) (Seq (Literal 'c') (Literal 'd')) ['c', 'd', 'a'] (Ok (['c', 'd'], ['a']))
  ```
-}
{-
Searches for the first pattern in the input. If the first pattern is matched, it returns the matched pattern and the
unmatched characters. If the first pattern isn't matched, then it moves onto the second pattern and repeats the same steps.
If the second pattern is found, it returns the matched pattern alongside the unmatched characters, otherwise it will
return an Err together with the input
-}
matchOneOf : RegexPattern -> RegexPattern -> List Char -> RegexResult
matchOneOf pat1 pat2 input =
    case (match pat1 input) of
        Ok (_, l1) -> Ok ((regexDeconstruct pat1), l1)
        Err (_) ->
            case (match pat2 input) of
                Ok (_, l2) -> Ok ((regexDeconstruct pat2), l2)
                Err (_) -> Err input

match : RegexPattern -> List Char -> RegexResult
match pattern input =
  case pattern of
    Literal char -> matchLit char input
    Many pat -> matchMany pat input
    OneOf pat1 pat2 -> matchOneOf pat1 pat2 input
    Seq pat1 pat2 -> matchSeq pat1 pat2 input