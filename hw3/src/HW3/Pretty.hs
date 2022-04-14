module HW3.Pretty
  (
    prettyValue
  ) where

import qualified Data.ByteString as BS
import qualified Data.Map as DM
import qualified Data.Text as T

import Data.Foldable (toList)
import Data.List (intercalate)
import Data.Ratio
import Data.Scientific
import Data.Word (Word8)
import HW3.Base
import Numeric (showHex)
import Prettyprinter
import Prettyprinter.Render.Terminal (AnsiStyle)

-- | Converts fraction to string.
fractionToString :: Show a => a -> a -> String
fractionToString num denom = show num ++ "/" ++ show denom

-- | Converts @Rational to string.
numberToString :: Rational -> String
numberToString number =
  let
    num = numerator number
    denom = denominator number
    checkFinite :: Integer -> Integer -> Integer
    checkFinite base x = 
      if (x `mod` base) /= 0
      then x
      else checkFinite base (x `div` base)
  in
    if ((checkFinite 5) $ (checkFinite 2 denom)) == 1
    then 
      if (denom == 1)
      then show num
      else 
        let
          (finiteValue, _) = fromRationalRepetendUnlimited number
        in formatScientific Fixed Nothing finiteValue
    else 
      if abs num < denom
      then fractionToString num denom
      else
        let
          (intPart, fracPart) = quotRem num denom
        in
          if fracPart > 0
          then (show intPart ++ " + " ++ fractionToString fracPart denom)
          else (show intPart ++ " - " ++ fractionToString (abs fracPart) denom)

-- | Converts list to string.
listToString :: [HiValue] -> String
listToString [] = "[ ]"
listToString lst =
  "[ "
  ++ intercalate ", " (map hiValueToString lst)
  ++ " ]"

-- | Converts dict to string.
dictToString :: [(HiValue, HiValue)] -> String
dictToString [] = "{ }"
dictToString lst =
  "{ "
  ++ intercalate
    ", "
    (map
      (\(a, b) -> hiValueToString a ++ ": " ++ hiValueToString b)
      lst)
  ++ " }"

-- | Converts @Word8 to string.
formatWord8 :: Word8 -> String
formatWord8 ch =
  let
    str = (showHex ch "")
  in
    if length str == 1
    then "0" ++ str
    else str

-- | Converts @Filepath to string.
formatFilepath :: FilePath -> String
formatFilepath str = "\"" ++ str ++ "\""

-- | Converts bytestring to string.
formatBytestring :: [Word8] -> String
formatBytestring [] = "[# #]"
formatBytestring bytes =
  "[# "
    ++ formatWord8 (head bytes)
    ++ foldl (\str element -> str ++ " " ++ formatWord8 element) "" (tail bytes)
    ++ " #]"

-- | Formats the action application.
formatAction :: String -> String -> String
formatAction name filepath = name ++ "(" ++ (formatFilepath filepath) ++ ")"

-- | Converts @HiValue to string.
hiValueToString :: HiValue -> String
hiValueToString (HiValueNumber number) = numberToString number
hiValueToString (HiValueBool True) = "true"
hiValueToString (HiValueBool False) = "false"
hiValueToString (HiValueFunction f) = show f
hiValueToString HiValueNull = "null"
hiValueToString (HiValueString text) = show text
hiValueToString (HiValueList lst) = listToString (toList lst)
hiValueToString (HiValueBytes bytestring) =
  formatBytestring (BS.unpack bytestring)
hiValueToString (HiValueAction action) =
  case action of
    (HiActionRead filepath) -> formatAction "read" filepath
    (HiActionWrite filepath content) ->
      "write("
      ++ formatFilepath filepath
      ++ ", "
      ++ formatBytestring (BS.unpack content)
      ++ ")"
    (HiActionMkDir filepath) -> formatAction "mkdir" filepath
    (HiActionChDir filepath) -> formatAction "cd" filepath
    HiActionCwd -> "cwd"
    HiActionNow -> "now"
    (HiActionRand start end) ->
      "rand(" ++ show start ++ ", " ++ show end ++ ")"
    (HiActionEcho text) -> formatAction "echo" (T.unpack text)
hiValueToString (HiValueTime time) = "parse-time(\"" ++ show time ++ "\")"
hiValueToString (HiValueDict dict) = dictToString (DM.toList dict)

-- | Prettifies @HiValue.
prettyValue :: HiValue -> Doc AnsiStyle
prettyValue = pretty . hiValueToString
