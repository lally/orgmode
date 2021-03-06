{-# LANGUAGE BangPatterns, GADTs, DeriveDataTypeable, StandaloneDeriving #-}

module Data.OrgMode.Text (
  LineNumber(..), toNumber, TextLine(..), isNumber, TextLineSource(..),
  normalizeInputText, lineAdd, linesStartingFrom, hasNumber, makeDrawerLines,
  wrapLine, prefixLine, tlPrint, tlFormat, wrapStringVarLines
  ) where

import Data.List (intercalate)
import Data.Monoid
import Data.Typeable
import Data.Generics (Generic(..))
import Data.Char (isSpace, toUpper, isPrint)
import Text.Printf
import System.IO

-- | Line numbers, where we can have an unattached root.
type LineNumber = Maybe Int

lineAdd Nothing _ = Nothing
lineAdd _ Nothing = Nothing
lineAdd (Just a) (Just b) = Just (a+b)

toNumber :: Int -> LineNumber -> Int
toNumber n Nothing = n
toNumber _ (Just a) = a

isNumber :: LineNumber -> Bool
isNumber Nothing = False
isNumber (Just _) = True

linesStartingFrom :: LineNumber -> [LineNumber]
linesStartingFrom Nothing = repeat Nothing
linesStartingFrom (Just l) = map Just [l..]

-- | Raw data about each line of text.  Lines with 'tlLineNum == None'
-- are generated and don't exist within the Org file (yet).
data TextLine = TextLine
                { tlIndent :: Int
                  -- ^how long of a whitespace (or asterisk, for 'Node') prefix is in tlText?
                , tlText :: String
                , tlLineNum :: LineNumber
                } deriving (Eq, Typeable)

hasNumber :: TextLine -> Bool
hasNumber (TextLine _ _ (Just _)) = True
hasNumber _ = False
formatLine tl =
  (printf "[%3d] " (tlIndent tl)) ++
  (printf "%-8s" $ (show $ tlLineNum tl)) ++ "|" ++ (tlText tl)

instance Show TextLine where
  show tl = "<" ++ (formatLine tl) ++ ">"

instance Ord TextLine where
  compare a b = compare (tlLineNum a) (tlLineNum b)

-- | Implements an API for getting text lines.  Useful for Org file
-- generation or mutation.
class TextLineSource s where
  getTextLines :: s -> [TextLine]

-- | Normalizes out newlines to UNIX format.  CR -> LF, CRLF -> LF
normalizeInputText :: String -> String
normalizeInputText text =
  -- Operators that work on reversed strings.
  let swapCrLf :: String -> Char -> String
      swapCrLf ('\r':cs) '\n' = '\n':cs
      swapCrLf ('\n':cs) '\r' = '\n':cs
      swapCrLf cs c = c:cs
      -- A good place for fixing unprintable chars, but we have to
      -- identify them.
      swapCr :: String -> Char -> String
      swapCr cs '\r' = '\n':cs
      swapCr cs c = c:cs
      revStr = reverse text
      swappedCrLf = foldl swapCrLf "" revStr
      swappedCr = foldl swapCr "" swappedCrLf
  in reverse swappedCr

trimEndOfLine :: String -> String
trimEndOfLine f = reverse $ dropWhile isSpace $ reverse f

wrapStringVarLines :: [Int] -> String -> String
wrapStringVarLines _ [] = []
wrapStringVarLines lens str
  | length str < (head lens) = str
  | otherwise =
    let first_word = takeWhile (not . isSpace) str
        len = head lens
        is_first_too_long = length first_word >= len
        wrapped_back =
          if is_first_too_long
          then first_word
          else reverse $ dropWhile (not . isSpace) $ reverse $ take len str
        remain = drop (length wrapped_back) str
    in if length wrapped_back > 0 || length remain > 0
       then wrapped_back ++ "\n" ++ wrapStringVarLines (drop 1 lens) remain
       else ""

wrapString :: Int -> String -> String
wrapString len str = wrapStringVarLines (repeat len) str
wrapLine :: Int -> TextLine -> [TextLine]
wrapLine width (TextLine indent string linenum) =
  let desired_len = width - indent
      strings = concatMap lines $ map (wrapString desired_len) $ lines string
      line_nrs = linesStartingFrom linenum
      makeTextLine (str, nr) = TextLine indent (trimEndOfLine str) nr
  in map makeTextLine $ zip strings line_nrs

prefixLine :: String -> TextLine -> TextLine
prefixLine pfx (TextLine indent string linenum) =
  let new_str = pfx ++ string
      new_indent = length $ takeWhile isSpace new_str
  in TextLine new_indent new_str linenum

makeDrawerLines :: LineNumber -> Int -> String -> [(String, String)] -> [TextLine]
makeDrawerLines fstLine depth name props =
  let !indent = take depth $ repeat ' '
      headline =
        TextLine depth (indent ++ ":" ++ (map toUpper name) ++ ":") fstLine
      mAdd  (Just x) y = Just (x + y)
      mAdd Nothing y = Nothing
      lastline =
        TextLine depth (indent ++ ":END:") (lineAdd fstLine $ Just (length props + 1))
      makePropLine ((prop, value), nr) =
        TextLine depth (indent ++ ":" ++ prop ++ ": " ++ value) (lineAdd fstLine $ Just nr)
      proplines = map makePropLine $ zip props [1..]
  in (headline:(proplines)) ++ [lastline]

tlFormat :: (TextLineSource s) => s -> String
tlFormat s =
  let lines = getTextLines s
  in intercalate "\n" $ map formatLine lines

tlPrint :: (TextLineSource s) => s -> IO ()
tlPrint s = putStrLn $ tlFormat s
