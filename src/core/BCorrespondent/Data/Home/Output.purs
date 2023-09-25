module BCorrespondent.Data.Home.Output ( Output (..) ) where

import Prelude

import Crypto.Jwt as Jwt

data Output = LoggedInSuccess Jwt.JwtUser | LoggedOutSuccess