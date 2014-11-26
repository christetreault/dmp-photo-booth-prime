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

main =
   runCoreMonad
      initCoreState
      photoboothMain

