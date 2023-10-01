module BCorrespondent.Data.ChildOutput.Home ( Output (..) ) where

import Prelude

import Crypto.Jwt as Jwt

data Output =
       LoggedInSuccess Jwt.JwtUser
     | LoggedOutSuccess 
     | PasswordResetLinkSend