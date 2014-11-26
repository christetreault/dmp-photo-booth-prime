{-|
Module : Persistence
Description : Persistence Module
Copyright : (c) Chris Tetreault, 2014

License : GPL-3
Stability : experimental

The persistence module is responsible for providing a mechanism for persisting
data. This may be configuration, or it may be any other persisted data.

The persistence module will implement a method to persist data. This data
can be anything, but will at the very least be module and program
configurations. The persistence module will implement persistence using the
@Persistable@ type defined within @DMP.Photobooth.Module.Types@, which is a list
of various typed properties.

This file represents the functions that must be implemented in order to
provide a complete implementation.

-}

module DMP.Photobooth.Module.Persistence where

import DMP.Photobooth.Module.Types
import DMP.Photobooth.Monads

{-|
   Persist a persistable. Accepts a source parameter to prevent name collisions
   between modules. Returns True if successful
-}
persist ::
   Source -- ^ The module requesting persistence
   -> String -- ^ The key to persist using
   -> Persistable
   -> ModuleT s IO Bool
persist s k p =
   undefined

{-|
   Request restoration of a persistable. If a value is found at the key, then
   it is returned, otherwise Nothing is returned
-}
restore ::
   Source -- ^ The module requesting resoration of a persistable
   -> String -- ^ The key to look up
   -> ModuleT s IO (Maybe Persistable)
restore s k =
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
