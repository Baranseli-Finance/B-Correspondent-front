module Buzgibi.Component.Auth.SignUp.PasswordCheker (check, Validation) where

import Prelude

type Validation = { id :: Int, value :: String, length :: Int, contains :: Array String }

foreign import check :: String -> Validation
