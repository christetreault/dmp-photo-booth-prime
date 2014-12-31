{-|
Module : Main
Copyright : (c) Chris Tetreault, 2014

License : GPL-3
Stability : experimental

The Main module implements the main function; the entry point to the program.
-}

module Main where

import DMP.Photobooth
import DMP.Photobooth.Monads
import DMP.Photobooth.Core
import System.Exit

{-|
The main function is the entry point to the program. The main function
initializes the program state, then runs the photoboothMain monadic action.

calls exitFailure if photoboothMain returns Nothing
-}
main :: IO ()
main =
   do
      coreState <-
         initCoreState
      result <-
         runCoreMonad
            coreState
            photoboothMain

      case result of
         Nothing -> exitFailure
         Just _ -> exitSuccess
