{-|
Module : Camera
Description : Camera Module
Copyright : (c) Chris Tetreault, 2014

License : GPL-3
Stability : experimental

This file represents the functions that must be implemented in order to
provide a complete implementation.
-}

module DMP.Photobooth.Module.Camera where

import qualified Data.ByteString.Lazy as BS
import DMP.Photobooth.Monads
import DMP.Photobooth.Module.Types

{-|
   Capture a photo.
-}
capture ::
   ModuleT s IO BS.ByteString
capture =
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
The initial state of the printer module, or Nothing
-}
initialState ::
   Maybe s
initialState =
   undefined
