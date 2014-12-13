{-|
Module : Photostrip
Description : Photostrip Module
Copyright : (c) Chris Tetreault, 2014

License : GPL-3
Stability : experimental

The Photostrip module exposes functions relating to transforming images into
a photostrip.

When the camera module begins capturing images, these images will be passed to
the photostrip module. At this point, it is the photostrip module's
responsability To transform these images into one single photostrip image.

This file represents the functions that must be implemented in order to
provide a complete implementation.

-}

module DMP.Photobooth.Module.Photostrip where

import qualified Data.ByteString.Lazy as BS
import DMP.Photobooth.Module.Types
import DMP.Photobooth.Monads


{-|
Transform a list of multiple foreground images into a completed photostrip.
All images are loaded into binary blobs
-}
process ::
   [BS.ByteString]
   -> ModuleT s IO BS.ByteString
process =
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
