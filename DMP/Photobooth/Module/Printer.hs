{-|
Module : Printer
Description : Printer Module
Copyright : (c) Chris Tetreault, 2014

License : GPL-3
Stability : experimental

The printer module exposes functions relating to printing a photostrip.

After the photostrip module transforms images captured by the camera module,
the finished strip is sent to the printer module to be printed.

This file represents the functions that must be implemented in order to
provide a complete implementation.
-}

module DMP.Photobooth.Module.Printer where

import DMP.Photobooth.Module.Types
import DMP.Photobooth.Monads
import qualified Data.ByteString.Lazy as BS

{-|
Print a photostrip.
-}
printStrip ::
   BS.ByteString -- ^ a strip to print, as a binary blob
   -> ModuleT s IO ()
printStrip b =
   undefined

{-|
Initializes the module with its configuration. Returns a Result
object that will contain the module's initial state

If the implementation needs to perform some imperative-style
"initialization", it should be done here. The return value of this function
will be stored by the core and used for this module's functions.
-}
init ::
   ModuleT s IO ()
init =
   undefined

{-|
Finalizes the module. If this module has any sort of resources that need
cleaning up, it should be done here.
-}
finalize ::
   ModuleT s IO ()
finalize =
   undefined

{-|
Request the default configuration of this module.
-}
defaultConfig ::
   Persistable
defaultConfig =
   undefined

{-|
The initial state of the printer module
-}
initialState ::
   Maybe s
initialState =
   undefined
