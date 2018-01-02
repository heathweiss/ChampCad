module Helpers.Applicative(extractE, extractMaybe) where

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
