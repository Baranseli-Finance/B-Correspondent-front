module Crypto.Jwt
  ( JwtClaims
  , JwtUser
  , parse
  ) where

import Prelude

import Effect (Effect)

-- {
--   "ident": 1,
--   "jwtclaims": {
--     "exp": 1698064862.067158,
--     "iat": 1695472862.067158
--   },
--   "jwtuuid": "08ad0f83-d3a2-49ee-9348-47bf57e7674e"
-- }
type JwtClaims = { exp :: Int, iat :: Int }

type JwtUser = { ident :: Int, jwtuuid :: String, jwtclaims :: JwtClaims }

foreign import parse :: String -> Effect JwtUser
