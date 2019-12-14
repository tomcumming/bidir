module TI where

import Control.Monad.State

import Type (ExtId)

type TIError = String

type TI = StateT ExtId (Either TIError)
