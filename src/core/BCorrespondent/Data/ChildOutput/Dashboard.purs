module BCorrespondent.Data.ChildOutput.Dashboard ( Output(..)) where

import Prelude

import Data.Maybe (Maybe)

data Output = 
       LoggedOut 
     | ResetPasswordTimeLeft Int
     | ResetPasswordOk