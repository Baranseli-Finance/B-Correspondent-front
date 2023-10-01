module BCorrespondent.Data.ChildOutput.ResetPasswordLink ( Output (..) ) where

import Prelude

import Effect.Exception (Error)


data Output = Server50x Error | PasswordNotChanged | PasswordOk