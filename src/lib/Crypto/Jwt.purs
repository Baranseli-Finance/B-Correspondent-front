module Crypto.Jwt
  ( JwtClaims
  , JwtUser
  , fetchInstitution
  , parse
  )
  where

import Prelude

import Effect (Effect)
import Foreign (Foreign, readInt, ForeignError)
import Control.Monad.Except.Trans (runExceptT)
import Data.List.Types (NonEmptyList)
import Data.Either (Either)

-- {
--   "ident": 1,
--   "jwtclaims": {
--     "exp": 1698064862.067158,
--     "iat": 1695472862.067158
--   },
--   "jwtuuid": "08ad0f83-d3a2-49ee-9348-47bf57e7674e",
--   "login": "..."
--   "instituion:" ..
-- }
type JwtClaims = { exp :: Int, iat :: Int }

type JwtUser = 
     { ident :: Int, 
       jwtuuid :: String, 
       login :: String, 
       jwtclaims :: JwtClaims,
       institution :: Foreign
     }

foreign import parse :: String -> Effect JwtUser

fetchInstitution :: forall m. Monad m => Foreign -> m (Either (NonEmptyList ForeignError) Int)
fetchInstitution = runExceptT <<< readInt 
