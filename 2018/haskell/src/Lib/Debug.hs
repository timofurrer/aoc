module Lib.Debug (debug) where

import qualified Debug.Trace as D

debug :: c -> String -> c
debug = flip D.trace
