{-# LANGUAGE OverloadedStrings #-}

module HW3.Evaluator
  (
    eval
  ) where

import qualified Data.ByteString as BS
import qualified Data.Map as DM
import qualified Data.Sequence as S
import qualified Data.Text as T

import HW3.Base

import Codec.Compression.Zlib
import Codec.Serialise
import Control.Monad.Except (ExceptT (..), lift, runExceptT)
import Data.ByteString.Lazy.Char8 (fromStrict, toStrict)
import Data.Foldable (toList)
import Data.Ratio
import Data.Semigroup (stimes)
import Data.Sequence (Seq, fromList, (><))
import Data.Text (Text)
import Data.Text.Encoding
import Data.Time.Clock (UTCTime, addUTCTime, diffUTCTime)
import Text.Read (readMaybe)

-- | Throws given @HiError.
throw :: Monad m => HiError -> ExceptT HiError m a
throw evalError = ExceptT (return (Left evalError))

-- | Asserts the given predicate is @True.
-- If @False, throws @HiErrorInvalidArgument.
check :: Monad m => Bool -> ExceptT HiError m ()
check True  = return ()
check False = throw HiErrorInvalidArgument

-- | Converts given @Rational to @Int.
-- If argument is not convertible to @Int, throws @HiErrorInvalidArgument.
intValue :: Monad m => Rational -> ExceptT HiError m Int
intValue ratio = do
    check ((denominator ratio) == 1)
    check ((numerator ratio) >= fromIntegral (minBound :: Int))
    check ((numerator ratio) <= fromIntegral (maxBound :: Int))
    return (fromIntegral (numerator ratio))

-- | Converts given @HiValue to @Int.
-- If argument is not convertible to @Int, throws @HiErrorInvalidArgument.
toInt :: Monad m => HiValue -> ExceptT HiError m Int
toInt (HiValueNumber number) = intValue number
toInt _                      = throw HiErrorInvalidArgument

-- | Maps @HiFun to its arity.
getArity :: HiFun -> Int
getArity func =
  case func of
    HiFunDiv            -> 2
    HiFunMul            -> 2
    HiFunAdd            -> 2
    HiFunSub            -> 2
    HiFunNot            -> 1
    HiFunAnd            -> 2
    HiFunOr             -> 2
    HiFunLessThan       -> 2
    HiFunNotGreaterThan -> 2
    HiFunGreaterThan    -> 2
    HiFunNotLessThan    -> 2
    HiFunEquals         -> 2
    HiFunNotEquals      -> 2
    HiFunIf             -> 3
    HiFunLength         -> 1
    HiFunToUpper        -> 1
    HiFunToLower        -> 1
    HiFunReverse        -> 1
    HiFunTrim           -> 1
    HiFunRange          -> 2
    HiFunFold           -> 2
    HiFunPackBytes      -> 1
    HiFunUnpackBytes    -> 1
    HiFunEncodeUtf8     -> 1
    HiFunDecodeUtf8     -> 1
    HiFunZip            -> 1
    HiFunUnzip          -> 1
    HiFunSerialise      -> 1
    HiFunDeserialise    -> 1
    HiFunRead           -> 1
    HiFunWrite          -> 2
    HiFunMkDir          -> 1
    HiFunChDir          -> 1
    HiFunParseTime      -> 1
    HiFunRand           -> 2
    HiFunEcho           -> 1
    HiFunCount          -> 1
    HiFunKeys           -> 1
    HiFunValues         -> 1
    HiFunInvert         -> 1
    _                   -> 0

-- | Checks if the list of arguments corresponds to function arity.
checkArity :: Monad m => HiFun -> [HiExpr] -> ExceptT HiError m ()
checkArity f args = do
  case (getArity f) of
    0 -> return ()
    arity ->
      if arity /= length args
      then throw HiErrorArityMismatch
      else return ()

-- | Checks if the given list is convertible to @Bytestring.
checkBytestring :: Monad m => [Int] -> ExceptT HiError m ()
checkBytestring [] = return ()
checkBytestring (number : rest) = do
  check (number >= 0 && number < 256)
  checkBytestring rest

-- | Applies unary function.
applyUnary :: Monad m => HiFun -> HiValue -> ExceptT HiError m HiValue
applyUnary HiFunNot (HiValueBool x) = return (HiValueBool (not x))
applyUnary HiFunLength (HiValueString text) =
  return (HiValueNumber (toRational $ T.length text))
applyUnary HiFunLength (HiValueList sq) =
  return (HiValueNumber (toRational $ S.length sq))
applyUnary HiFunLength (HiValueBytes bytestring) =
  return (HiValueNumber (toRational $ BS.length bytestring))
applyUnary HiFunToUpper (HiValueString text) =
  return (HiValueString (T.toUpper text))
applyUnary HiFunToLower (HiValueString text) =
  return (HiValueString (T.toLower text))
applyUnary HiFunReverse (HiValueString text) =
  return (HiValueString (T.reverse text))
applyUnary HiFunReverse (HiValueList sq) =
  return (HiValueList (S.reverse sq))
applyUnary HiFunReverse (HiValueBytes bytestring) =
  return (HiValueBytes (BS.reverse bytestring))
applyUnary HiFunTrim (HiValueString text) =
  return (HiValueString (T.strip text))
applyUnary HiFunPackBytes (HiValueList sq) = do
  intValues <- mapArgs toInt (toList sq)
  checkBytestring intValues
  return (HiValueBytes $ BS.pack (map fromIntegral intValues))
applyUnary HiFunUnpackBytes (HiValueBytes bytestring) =
    return
      (HiValueList
        (fromList (map (HiValueNumber . toRational) (BS.unpack bytestring))))
applyUnary HiFunEncodeUtf8 (HiValueString text) =
  return (HiValueBytes $ encodeUtf8 text)
applyUnary HiFunDecodeUtf8 (HiValueBytes bytestring) =
  case (decodeUtf8' bytestring) of
    (Left _)     -> return HiValueNull
    (Right text) -> return (HiValueString text)
applyUnary HiFunZip (HiValueBytes bytestring) =
  return
    (HiValueBytes $
      toStrict
        (compressWith
          defaultCompressParams {
            compressLevel = bestCompression
          }
          (fromStrict bytestring)))
applyUnary HiFunUnzip (HiValueBytes bytestring) =
  return
    (HiValueBytes $
      toStrict
        (decompress (fromStrict bytestring)))
applyUnary HiFunSerialise hiValue =
  return (HiValueBytes (toStrict (serialise hiValue)))
applyUnary HiFunDeserialise (HiValueBytes bytestring) =
  return (deserialise (fromStrict bytestring))
applyUnary HiFunRead (HiValueString filepath) =
  return (HiValueAction (HiActionRead (T.unpack filepath)))
applyUnary HiFunMkDir (HiValueString filepath) =
  return (HiValueAction (HiActionMkDir (T.unpack filepath)))
applyUnary HiFunChDir (HiValueString filepath) =
  return (HiValueAction (HiActionChDir (T.unpack filepath)))
applyUnary HiFunParseTime (HiValueString str) = do
  case (readMaybe (T.unpack str) :: Maybe UTCTime) of
    Nothing     -> return HiValueNull
    (Just time) -> return (HiValueTime time)
applyUnary HiFunEcho (HiValueString text) =
  return (HiValueAction $ HiActionEcho text)
applyUnary HiFunKeys (HiValueDict dict) =
  return (HiValueList $ fromList $ DM.keys dict)
applyUnary HiFunValues (HiValueDict dict) =
  return (HiValueList $ fromList $ DM.elems dict)
applyUnary HiFunCount (HiValueList lst) =
  HiValueDict <$> (count (toList lst))
applyUnary HiFunCount (HiValueString text) =
  (HiValueDict . DM.mapKeys (HiValueString . T.singleton))
  <$> (count (T.unpack text))
applyUnary HiFunCount (HiValueBytes bytestring) =
  (HiValueDict . DM.mapKeys (HiValueNumber . toRational))
  <$> (count (BS.unpack bytestring))
applyUnary HiFunInvert (HiValueDict dict) =
  return $
    HiValueDict $
      DM.map
      (HiValueList . fromList)
      (DM.fromListWith
        (++)
        (map (\(a, b) -> (b, [a])) (DM.toAscList dict)))
applyUnary _ _ = throw HiErrorInvalidArgument

-- | Repeats argument, which has instance of @Monoid, the given number of times.
repeatTimes :: (Monad m, Monoid a) => HiValue -> a -> ExceptT HiError m a
repeatTimes number ma = do
  times <- toInt number
  if times <= 0
  then throw HiErrorInvalidArgument
  else return (stimes times ma)

-- | Applies binary function.
applyBinary :: Monad m => HiFun -> HiValue -> HiValue -> ExceptT HiError m HiValue
applyBinary HiFunDiv _ (HiValueNumber 0) = throw HiErrorDivideByZero
applyBinary HiFunDiv (HiValueNumber x) (HiValueNumber y) =
  return (HiValueNumber (x / y))
applyBinary HiFunDiv (HiValueString first) (HiValueString second) =
  return (HiValueString $ T.concat [first, T.pack "/", second])
applyBinary HiFunMul (HiValueNumber x) (HiValueNumber y) =
  return (HiValueNumber (x * y))
applyBinary HiFunMul (HiValueString str) number =
  HiValueString <$> (repeatTimes number str)
applyBinary HiFunMul (HiValueList sq) number =
  HiValueList <$> (repeatTimes number sq)
applyBinary HiFunMul (HiValueBytes bytestring) number =
  HiValueBytes <$> (repeatTimes number bytestring)
applyBinary HiFunAdd (HiValueNumber x) (HiValueNumber y) =
  return (HiValueNumber (x + y))
applyBinary HiFunAdd (HiValueString first) (HiValueString second) =
  return (HiValueString (T.concat [first, second]))
applyBinary HiFunAdd (HiValueList first) (HiValueList second) =
  return (HiValueList (first >< second))
applyBinary HiFunAdd (HiValueBytes first) (HiValueBytes second) =
  return (HiValueBytes (BS.append first second))
applyBinary HiFunAdd (HiValueTime time) (HiValueNumber diff) =
  return (HiValueTime (addUTCTime (fromRational diff) time))
applyBinary HiFunSub (HiValueNumber x) (HiValueNumber y) =
  return (HiValueNumber (x - y))
applyBinary HiFunSub (HiValueTime first) (HiValueTime second) =
  return (HiValueNumber $ toRational (diffUTCTime first second))
applyBinary HiFunEquals left right = return (HiValueBool (left == right))
applyBinary HiFunNotEquals left right = return (HiValueBool (left /= right))
applyBinary HiFunLessThan left right = return (HiValueBool (left < right))
applyBinary HiFunNotGreaterThan left right = return (HiValueBool (left <= right))
applyBinary HiFunGreaterThan left right = return (HiValueBool (left > right))
applyBinary HiFunNotLessThan left right = return (HiValueBool (left >= right))
applyBinary HiFunRange (HiValueNumber start) (HiValueNumber end) =
  return (HiValueList $ fromList (map HiValueNumber [start..end]))
applyBinary HiFunWrite (HiValueString filepath) (HiValueString content) =
  return (HiValueAction (HiActionWrite (T.unpack filepath) (encodeUtf8 content)))
applyBinary HiFunRand first second = do
  start <- toInt first
  end <- toInt second
  return (HiValueAction $ HiActionRand start end)
applyBinary _ _ _ = throw HiErrorInvalidArgument

-- | Takes slice of the given list.
slice :: Monad m => Rational -> Rational -> [a] -> ExceptT HiError m [a]
slice left right lst = do
  start <- getIndex (length lst) left
  end <- getIndex (length lst) right
  if start >= end
  then return []
  else return (take (end - start) $ drop start lst)

-- | Count function implementation.
count :: (Monad m, Ord a) => [a] -> ExceptT HiError m (DM.Map a HiValue)
count lst =
  return $
    DM.map
      (HiValueNumber . toRational) $
      foldr
      (\element mp ->
        DM.insertWithKey (\_ old new -> old + new) element (1 :: Int) mp)
      DM.empty
      lst

-- | Converts @Rational to the valid integer index.
getIndex :: Monad m => Int -> Rational -> ExceptT HiError m Int
getIndex delta x =
  (\ind ->
    if ind < 0
    then 0
    else
      if ind >= delta
      then delta
      else ind)
    <$> ((\ind ->
            if ind < 0
            then ind + delta
            else ind)
      <$> intValue x)

-- | Looks up given list by index.
lookupByIndex 
  :: Monad m 
  => (a -> HiValue) 
  -> [a] 
  -> HiValue 
  -> ExceptT HiError m HiValue
lookupByIndex pack lst (HiValueNumber ratio) = do
  index <- intValue ratio
  if index < 0 || index >= length lst
  then return HiValueNull
  else return (pack (lst !! index))
lookupByIndex _ _ _ = throw HiErrorInvalidArgument

-- | Looks up given text by index.
stringLookupByIndex :: Monad m => Text -> HiValue -> ExceptT HiError m HiValue
stringLookupByIndex text number =
  lookupByIndex (HiValueString . T.singleton) (T.unpack text) number

-- | Looks up given sequence by index.
listLookupByIndex 
  :: Monad m 
  => Seq HiValue 
  -> HiValue 
  -> ExceptT HiError m HiValue
listLookupByIndex sq number =
  lookupByIndex id (toList sq) number

-- | Looks up given bytestring by index.
bytestringLookupByIndex 
  :: Monad m 
  => BS.ByteString 
  -> HiValue 
  -> ExceptT HiError m HiValue
bytestringLookupByIndex bytestring number =
  lookupByIndex (HiValueNumber . toRational) (BS.unpack bytestring) number

-- | Returns length of the given list, or text or bytestring.
getLength :: Monad m => HiValue -> ExceptT HiError m Rational
getLength (HiValueList sq)          = return (toRational $ length sq)
getLength (HiValueString text)      = return (toRational $ T.length text)
getLength (HiValueBytes bytestring) = return (toRational $ BS.length bytestring)
getLength _                         = throw HiErrorInvalidArgument

-- | Slice function implementation for @HiValue.
takeSlice :: Monad m => Rational -> Rational -> HiValue -> ExceptT HiError m HiValue
takeSlice left right (HiValueList sq) =
  (HiValueList . fromList)
  <$> (slice left right (toList sq))
takeSlice left right (HiValueString text) =
  (HiValueString . T.pack)
  <$> (slice left right (T.unpack text))
takeSlice left right (HiValueBytes bytestring) =
  (HiValueBytes . BS.pack)
  <$> (slice left right (BS.unpack bytestring))
takeSlice _ _ _ = throw HiErrorInvalidArgument

-- | Applies slice function to the given @HiValue.
applySlice :: Monad m => HiValue -> HiValue -> HiValue -> ExceptT HiError m HiValue
applySlice (HiValueNumber start) (HiValueNumber end) lst =
  takeSlice start end lst
applySlice left HiValueNull lst = do
  len <- getLength lst
  case left of
    (HiValueNumber start) -> takeSlice start len lst
    HiValueNull           -> takeSlice 0 len lst
    _                     -> throw HiErrorInvalidArgument
applySlice HiValueNull (HiValueNumber end) lst =
  takeSlice 0 end lst
applySlice _ _ _ = throw HiErrorInvalidArgument

-- | Map function implementation.
-- | Takes function returning ExceptT monad and a list.
-- | Applies function to the given list
-- and returns list wrapped into ExceptT monad.
mapArgs :: Monad m => (a -> ExceptT HiError m b) -> [a] -> ExceptT HiError m [b]
mapArgs _ [] = return []
mapArgs f (x : xs) = do
  value <- f x
  rest <- mapArgs f xs
  return (value : rest)

-- | Evaluates list of @HiExpr.
evaluateArgs :: HiMonad m => [HiExpr] -> ExceptT HiError m [HiValue]
evaluateArgs = mapArgs evaluate

-- | Fold implementation for binary functions.
fold :: HiMonad m => HiValue -> [HiValue] -> ExceptT HiError m HiValue
fold _ [] = return HiValueNull
fold _ [res] = return res
fold f (first : second : rest) = do
  value <- evaluate $
    HiExprApply (HiExprValue f) [(HiExprValue first), (HiExprValue second)]
  fold f (value : rest)

-- | Applies fold function.
applyFold :: HiMonad m => HiValue -> [HiValue] -> ExceptT HiError m HiValue
applyFold (HiValueFunction function) args
  | (getArity function) /= 2 = throw HiErrorInvalidArgument
  | otherwise = fold (HiValueFunction function) args
applyFold op args = fold op args

-- | Evaluates the given @HiExpr. Returns value in @ExceptT monad.
evaluate :: HiMonad m => HiExpr -> ExceptT HiError m HiValue
evaluate (HiExprValue value) = return value
evaluate (HiExprApply (HiExprValue (HiValueFunction HiFunIf)) args) = do
  checkArity HiFunIf args
  cond <- evaluate (head args)
  case cond of
    (HiValueBool True)  -> evaluate (args !! 1)
    (HiValueBool False) -> evaluate (args !! 2)
    _                   -> throw HiErrorInvalidArgument
evaluate (HiExprApply (HiExprValue (HiValueFunction HiFunList)) args) = do
  evaluatedArgs <- evaluateArgs args
  return (HiValueList (fromList evaluatedArgs))
evaluate (HiExprApply (HiExprValue (HiValueFunction HiFunAnd)) args) = do
  checkArity HiFunAnd args
  first <- evaluate (head args)
  case first of
    HiValueNull         -> return first
    (HiValueBool False) -> return first
    _                   -> evaluate (args !! 1)
evaluate (HiExprApply (HiExprValue (HiValueFunction HiFunOr)) args) = do
  checkArity HiFunOr args
  first <- evaluate (head args)
  case first of
    HiValueNull         -> evaluate (args !! 1)
    (HiValueBool False) -> evaluate (args !! 1)
    _                   -> return first
evaluate (HiExprApply (HiExprValue (HiValueFunction HiFunFold)) args) = do
  checkArity HiFunFold args
  toApply <- evaluate (head args)
  arguments <- evaluate (args !! 1)
  case arguments of
    (HiValueList sq) -> applyFold toApply (toList sq)
    _                -> throw HiErrorInvalidArgument
evaluate (HiExprApply (HiExprValue (HiValueFunction f)) args) = do
  checkArity f args
  evaluatedArgs <- evaluateArgs args
  case (getArity f) of
    1 -> applyUnary f (head evaluatedArgs)
    2 -> applyBinary f (head evaluatedArgs) (evaluatedArgs !! 1)
    _ -> throw HiErrorInvalidFunction
evaluate (HiExprApply (HiExprValue (HiValueString text)) args) = do
  evaluatedArgs <- evaluateArgs args
  case (length args) of
   1 -> stringLookupByIndex text (head evaluatedArgs)
   2 -> applySlice
    (head evaluatedArgs) (evaluatedArgs !! 1) (HiValueString text)
   _ -> throw HiErrorArityMismatch
evaluate (HiExprApply (HiExprValue (HiValueList lst)) args) = do
  evaluatedArgs <- evaluateArgs args
  case (length args) of
    1 -> listLookupByIndex lst (head evaluatedArgs)
    2 -> applySlice (head evaluatedArgs) (evaluatedArgs !! 1) (HiValueList lst)
    _ -> throw HiErrorArityMismatch
evaluate (HiExprApply (HiExprValue (HiValueBytes bytestring)) args) = do
  evaluatedArgs <- evaluateArgs args
  case (length args) of
    1 -> bytestringLookupByIndex bytestring (head evaluatedArgs)
    2 -> applySlice
      (head evaluatedArgs) (evaluatedArgs !! 1) (HiValueBytes bytestring)
    _ -> throw HiErrorArityMismatch
evaluate (HiExprApply (HiExprValue (HiValueDict dict)) args) = do
  evaluatedArgs <- evaluateArgs args
  case (length args) of
    1 ->
      case (DM.lookup (head evaluatedArgs) dict) of
        Nothing      -> return HiValueNull
        (Just value) -> return value
    _ -> throw HiErrorArityMismatch
evaluate (HiExprApply (HiExprValue _) _) = throw HiErrorInvalidFunction
evaluate (HiExprApply expr args) = do
    f <- evaluate expr
    evaluate (HiExprApply (HiExprValue f) args)
evaluate (HiExprRun expr) = do
  value <- evaluate expr
  case value of
    (HiValueAction action) -> lift (runAction action)
    _                      -> throw HiErrorInvalidArgument
evaluate (HiExprDict dict) = do
  keys <- evaluateArgs (map fst dict)
  values <- evaluateArgs (map snd dict)
  return (HiValueDict (DM.fromList (zip keys values)))

-- | Evaluates the given @HiExpr. Returns value wrapped into @HiMonad.
eval :: HiMonad m => HiExpr -> m (Either HiError HiValue)
eval expr = runExceptT (evaluate expr)

