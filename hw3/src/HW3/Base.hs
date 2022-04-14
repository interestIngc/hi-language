{-# LANGUAGE DeriveGeneric #-}

module HW3.Base
  (
    HiFun (..)
  , HiAction (..)
  , HiValue (..)
  , HiExpr (..)
  , HiError (..)
  , HiMonad (..)
  ) where

import Codec.Serialise
import Data.ByteString (ByteString)
import Data.Map.Internal (Map)
import Data.Sequence.Internal (Seq)
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import GHC.Generics

-- | Data type representing function names.
data HiFun =
  HiFunDiv
  | HiFunMul
  | HiFunAdd
  | HiFunSub
  | HiFunNot
  | HiFunAnd
  | HiFunOr
  | HiFunLessThan
  | HiFunGreaterThan
  | HiFunEquals
  | HiFunNotLessThan
  | HiFunNotGreaterThan
  | HiFunNotEquals
  | HiFunIf
  | HiFunLength
  | HiFunToUpper
  | HiFunToLower
  | HiFunReverse
  | HiFunTrim
  | HiFunList
  | HiFunRange
  | HiFunFold
  | HiFunPackBytes
  | HiFunUnpackBytes
  | HiFunEncodeUtf8
  | HiFunDecodeUtf8
  | HiFunZip
  | HiFunUnzip
  | HiFunSerialise
  | HiFunDeserialise
  | HiFunRead
  | HiFunWrite
  | HiFunMkDir
  | HiFunChDir
  | HiFunParseTime
  | HiFunRand
  | HiFunEcho
  | HiFunCount
  | HiFunKeys
  | HiFunValues
  | HiFunInvert
  deriving (Eq, Enum, Ord, Generic)

-- | Data type representing actions.
data HiAction =
    HiActionRead FilePath
  | HiActionWrite FilePath ByteString
  | HiActionMkDir FilePath
  | HiActionChDir FilePath
  | HiActionCwd
  | HiActionNow
  | HiActionRand Int Int
  | HiActionEcho Text
  deriving (Eq, Show, Generic)

-- | Data type representing values.
data HiValue =
    HiValueNull
  | HiValueBool Bool
  | HiValueNumber Rational
  | HiValueFunction HiFun
  | HiValueString Text
  | HiValueList (Seq HiValue)
  | HiValueBytes ByteString
  | HiValueAction HiAction
  | HiValueTime UTCTime
  | HiValueDict (Map HiValue HiValue)
  deriving (Eq, Show, Generic)

-- | Data type representing expressions.
data HiExpr =
    HiExprValue HiValue
  | HiExprApply HiExpr [HiExpr]
  | HiExprRun HiExpr
  | HiExprDict [(HiExpr, HiExpr)]
  deriving (Eq, Show)

-- | Data type representing evaluation errors.
data HiError =
  HiErrorInvalidArgument
  | HiErrorInvalidFunction
  | HiErrorArityMismatch
  | HiErrorDivideByZero
  deriving (Eq, Show)

instance Ord HiValue where
  HiValueNull <= _                            = True
  _ <= HiValueNull                            = False
  (HiValueBool x) <= (HiValueBool y)          = x <= y
  (HiValueBool _) <= _                        = True
  _ <= (HiValueBool _)                        = False
  (HiValueNumber x) <= (HiValueNumber y)      = x <= y
  (HiValueNumber _) <= _                      = True
  _ <= (HiValueNumber _)                      = False
  (HiValueFunction f) <= (HiValueFunction g)  = f <= g
  (HiValueFunction _) <= _                    = True
  _ <= (HiValueFunction _)                    = False
  (HiValueString p) <= (HiValueString q)      = p <= q
  (HiValueString _) <= _                      = True
  _ <= (HiValueString _)                      = False
  (HiValueList left) <= (HiValueList right)   = left <= right
  (HiValueList _) <= _                        = True
  _ <= (HiValueList _)                        = False
  (HiValueBytes left) <= (HiValueBytes right) = left <= right
  (HiValueBytes _) <= _                       = True
  _ <= (HiValueBytes _)                       = False
  (HiValueAction _) <= (HiValueAction _)      = True
  (HiValueAction _) <= _                      = True
  _ <= (HiValueAction _)                      = False
  (HiValueTime left) <= (HiValueTime right)   = left <= right
  (HiValueTime _) <= _                        = True
  _ <= (HiValueTime _)                        = False
  (HiValueDict left) <= (HiValueDict right)   = left <= right

instance Show HiFun where
  show HiFunDiv            = "div"
  show HiFunMul            = "mul"
  show HiFunAdd            = "add"
  show HiFunSub            = "sub"
  show HiFunNot            = "not"
  show HiFunAnd            = "and"
  show HiFunOr             = "or"
  show HiFunLessThan       = "less-than"
  show HiFunGreaterThan    = "greater-than"
  show HiFunEquals         = "equals"
  show HiFunNotLessThan    = "not-less-than"
  show HiFunNotGreaterThan = "not-greater-than"
  show HiFunNotEquals      = "not-equals"
  show HiFunIf             = "if"
  show HiFunLength         = "length"
  show HiFunToUpper        = "to-upper"
  show HiFunToLower        = "to-lower"
  show HiFunReverse        = "reverse"
  show HiFunTrim           = "trim"
  show HiFunList           = "list"
  show HiFunRange          = "range"
  show HiFunFold           = "fold"
  show HiFunPackBytes      = "pack-bytes"
  show HiFunUnpackBytes    = "unpack-bytes"
  show HiFunEncodeUtf8     = "encode-utf8"
  show HiFunDecodeUtf8     = "decode-utf8"
  show HiFunZip            = "zip"
  show HiFunUnzip          = "unzip"
  show HiFunSerialise      = "serialise"
  show HiFunDeserialise    = "deserialise"
  show HiFunRead           = "read"
  show HiFunWrite          = "write"
  show HiFunMkDir          = "mkdir"
  show HiFunChDir          = "cd"
  show HiFunParseTime      = "parse-time"
  show HiFunRand           = "rand"
  show HiFunEcho           = "echo"
  show HiFunCount          = "count"
  show HiFunKeys           = "keys"
  show HiFunValues         = "values"
  show HiFunInvert         = "invert"

instance Serialise HiFun
instance Serialise HiAction
instance Serialise HiValue

-- | Type class representing monads that can execute @HiAction.
class Monad m => HiMonad m where
  runAction :: HiAction -> m HiValue
