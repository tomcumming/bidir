module Bidir.TI where

import Control.Monad.State

import Bidir.Type (ExtId)

type TIError = String

type TI = StateT ExtId (Either TIError)

fresh :: TI ExtId
fresh = do
    x <- get
    put (succ x)
    return x
