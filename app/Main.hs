module Main where

import Control.Monad
import Control.Monad.Trans.Except
import Control.Monad.Trans.Class

import Text.Regex.TDFA
import Text.Regex.TDFA.Text ()

--Reimplemntation of suspect1.png
data RegRequest  = RegRequest {
            user_name :: Maybe String
           ,user_password_new :: Maybe String
           ,user_password_repeat :: Maybe String
           ,user_email :: Maybe String
        }

validateRegistration :: RegRequest -> IO () -> ExceptT String IO ()
validateRegistration r successAction = do
                            usernameExists r
                            passwordExists r
                            matchingPasswords r
                            pwLengthValid r
                            usernameLengthValid r
                            usernameRegexValid r
                            usernameDoesNotExist r
                            emailExists r
                            emailValidLength r
                            validEmail r
                            lift successAction

usernameExists :: RegRequest -> ExceptT String IO ()
usernameExists r  = case user_name r of
                        Just _ -> return ()
                        Nothing -> throwE "Empty Username"

passwordExists r = case user_password_new r of
                        Just _ -> return ()
                        Nothing -> throwE "Empty Password"

matchingPasswords r = if user_password_new r == user_password_repeat r
                      then return ()
                      else throwE "Passwords do not match"

pwLengthValid r = case user_password_new r of
                Just s -> if length s > 5
                          then return ()
                          else throwE err
                Nothing -> throwE err
    where err = "Password must be at least 6 characters"

usernameLengthValid r = case user_name r of
                            Just s -> if length s > 1 && length s < 65
                                      then return ()
                                      else throwE err
                            Nothing -> throwE err
    where err = "Username must be between 2 and 64 characters"


usernameRegexValid :: RegRequest -> ExceptT String IO ()
usernameRegexValid r = case user_name r of
                            Just s -> if s =~ usernameRegex
                                      then return ()
                                      else throwE err
                            Nothing -> throwE err
    where usernameRegex = "[a-zA-Z0-9]{2,64}"
          err = "Username must be only a-z, A-Z, 0-9"

usernameDoesNotExist :: RegRequest -> ExceptT String IO ()
usernameDoesNotExist r = case user_name r of
                            Just s -> Control.Monad.when (isset $ user_name r) $ throwE err
                            Nothing -> throwE err
        where isset = return False
              err = "Username already exists"

emailExists :: RegRequest -> ExceptT String IO ()
emailExists r = case user_email r of
                    Just s -> return ()
                    Nothing -> throwE "Email cannot be empty"

emailValidLength :: RegRequest -> ExceptT String IO ()
emailValidLength r = case  user_email r of
                        Just s -> if length s < 65
                                  then return ()
                                  else throwE err
                        Nothing -> throwE err
    where err = "Email must be less than 64 characters"

validEmail :: RegRequest -> ExceptT String IO ()
validEmail r = case user_email r of
                    Just s -> return () --There's really some sort of regex for this here
                    Nothing -> throwE "YOu must provide a valid email address"

main :: IO ()
main = putStrLn "Hello, Haskell!"
