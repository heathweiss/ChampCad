module Helpers.Applicative(extractE, extractMaybe, removeMaybe, appendE) where

--when applicatively calling a fx that takes non-either vals but returns Either val
  --so the input vals are Either, but are extracted with applicative.
  --The result is an Either String (Either String a) which has an extra layer of either.
extractE :: (Either String (Either String a)) -> Either String a
extractE (Left e) = Left e
extractE (Right (Left e)) = Left e
extractE (Right (Right val)) = Right val

--has not been tested.
--Did not work 1 time I tried it. Not sure why, so did not use it.
extractMaybe :: (Maybe (Maybe a)) -> Maybe a
extractMaybe Nothing = Nothing
extractMaybe (Just Nothing) = Nothing
extractMaybe (Just (Just val)) = Just val

-- | If the embedded object is a Just, return it as a Right object
-- | If it is nothing, return it as Left. Supply a message.
removeMaybe :: String -> Either String (Maybe a) -> Either String a
removeMaybe message (Right (Just a)) = Right a
removeMaybe message (Right Nothing) = Left message
removeMaybe message (Left e) = Left $ "removeMaybe: " ++ message ++ " e: " ++ e

-- | Append an Either value onto a list of that value.
-- | If a Right value, append it and return a Right [value]
-- | Otherwise return a Left e
appendE :: Either String a -> [a] -> Either String [a]
appendE (Left e) _ = Left $ "appendE error: "  ++ e
appendE (Right a) list = Right $ a:list
