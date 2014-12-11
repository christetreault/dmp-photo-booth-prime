{-|
Module : Main
Copyright : (c) Chris Tetreault, 2014

License : GPL-3
Stability : experimental
-}

module Main where

import DMP.Photobooth
import DMP.Photobooth.Monads
import DMP.Photobooth.Core

main :: IO ()
main =
   do
      coreState <-
         initCoreState
      coreResult <-
         runCoreMonad
            coreState
            photoboothMain

      return ()
