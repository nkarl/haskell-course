{-# LANGUAGE ScopedTypeVariables #-}

module Homework15B where

import System.Directory (doesFileExist)

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- IMPORTANT: Read the README.md file before completing the homework.
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

-- 1. Write a function that takes a list and returns the head if the list is not empty.
-- If the list is empty, return Nothing.

headMaybe :: [a] -> Maybe a
headMaybe [] = Nothing
headMaybe (x : _) = Just x

-- 2. Write a function that takes a list of Maybe values and returns a list of all the Just values.
-- If there are no Just values, return an empty list.

catMaybes :: [Maybe a] -> [a]
catMaybes [] = []
catMaybes (y : ys) = case y of
  Nothing -> catMaybes ys
  Just a -> a : catMaybes ys

-- 3. Write a function that tries to read from a file and returns the contents of the file.
-- If the file does not exist, return Nothing.

readFileMaybe :: FilePath -> IO (Maybe String)
readFileMaybe path = do
  isFile <- doesFileExist path
  if isFile
    then
      readFile path >>= \content -> case content of
        "" -> pure Nothing
        _ -> pure $ Just content
    else
      pure Nothing

-- 4. Write a function that checks all the requirements for a password using the
-- Either type with a custom data type for errors.
-- The requirements are:
-- - The password must be at least 10 characters long.
-- - The password must contain at least one digit.
-- - The password must contain at least one uppercase letter.
-- - The password must contain at least one lowercase letter.

data PasswordError
  = NotLongEnough
  | NoDigit
  | NoUpperCase
  | NoLowerCase
  deriving (Show, Eq)

passwordLongEnough :: String -> Either PasswordError String
passwordLongEnough passwd =
  let x = length passwd
   in if x >= 10 then Right passwd else Left NotLongEnough

passwordHasDigit :: String -> Either PasswordError String
passwordHasDigit passwd =
  if any (`elem` ['0' .. '9']) passwd
    then Right passwd
    else Left NoDigit

passwordHasUppercase :: String -> Either PasswordError String
passwordHasUppercase passwd =
  if any (`elem` ['A' .. 'Z']) passwd
    then Right passwd
    else Left NoUpperCase

passwordHasLowercase :: String -> Either PasswordError String
passwordHasLowercase passwd =
  if any (`elem` ['a' .. 'z']) passwd
    then Right passwd
    else Left NoLowerCase

passwordRequirements :: String -> Either PasswordError String
passwordRequirements passwd =
  passwordLongEnough passwd
    >>= passwordHasDigit
    >>= passwordHasUppercase
    >>= passwordHasLowercase
