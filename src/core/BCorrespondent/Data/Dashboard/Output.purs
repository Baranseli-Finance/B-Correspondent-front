module BCorrespondent.Data.Dashboard.Output ( Output(..)) where

import Prelude

import Data.Maybe (Maybe)

data Output = 
       LoggedOut 
     | ResetPasswordTimeLeft Int
     | ResetPasswordOk