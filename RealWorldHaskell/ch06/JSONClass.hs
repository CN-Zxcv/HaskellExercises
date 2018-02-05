{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverlappingInstances #-}

import Control.Arrow (second)

import SimpleJSON

type JSONError = String

class JSON a where
    toJValue :: a -> JValue
    fromJValue :: JValue -> Either JSONError a

instance JSON JValue where
    toJValue = id
    fromJValue = Right

instance JSON Bool where
    toJValue = JBool
    fromJValue (JBool b) = Right b
    fromJValue _ = Left "not a JSON boolean"

instance JSON String where
    toJValue = JString
    fromJValue (JString s) = Right s
    fromJValue _ = Left "not a JSON string"

doubleToJValue :: (Double -> a) -> JValue -> Either JSONError a
doubleToJValue f (JNumber v) = Right (f v)
doubleToJValue _ _ = Left "not a JSON number"

instance JSON Int where
    toJValue = JNumber . realToFrac
    fromJValue = doubleToJValue round

instance JSON Integer where
    toJValue = JNumber . realToFrac
    fromJValue = doubleToJValue round

instance JSON Double where
    toJValue = JNumber
    fromJValue = doubleToJValue id


instance (JSON a) => JSON [a] where
    toJValue = JArray . map toJValue
    fromJValue (JArray a) = mapEither fromJValue a
    fromJValue _ = Left "not a JSON array"

instance (JSON a) => JSON [(String, a)] where
    toJValue = JObject . map (second toJValue)
    fromJValue (JObject a) = mapEither unwrap a
        where
            unwrap (k, v) = whenRight ((,) k) (fromJValue v)
    fromJValue _ = Left "not a JSON object"

whenRight :: (b -> c) -> Either a b -> Either a c
whenRight _ (Left err) = Left err
whenRight f (Right a) = Right (f a)

mapEither :: (a -> Either b c) -> [a] -> Either b [c]
mapEither f (x:xs) = case mapEither f xs of
    Left err -> Left err
    Right ys -> case f x of
        Left err -> Left err
        Right y -> Right (y:ys)
mapEither _ _ = Right []
