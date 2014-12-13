{-|
Module : Trigger
Description : Trigger Module
Copyright : (c) Chris Tetreault, 2014

License : GPL-3
Stability : experimental

The trigger module exposes functions relating to the triggering of the capture
process to the Core.

The trigger module controls the triggering process. When
the user "presses the button", the trigger module is responsible for detecting
that this has occurred, and signaling the core to begin the capture process.

The "trigger" itself is a physical device. To this end, the trigger module is
responsible for communicating to the trigger. The trigger module implements
functions to tell the trigger various things such as a capture countdown, or
that the photobooth is processing a photo strip or printing.

This file represents the functions that must be implemented in order to
provide a complete implementation.
-}

module DMP.Photobooth.Module.Trigger where

import DMP.Photobooth.Module.Types
import DMP.Photobooth.Monads

{-|
Messages from the trigger to the core
-} -- TODO: Do I need this?
--data TriggerMessage =
--   Initiate |
--   CountdownComplete

{-|
Listen for the trigger. When the trigger messages the trigger module, this
function returns with the message recieved
-}
listen ::
   ModuleT s IO ()
listen =
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

{-|
Countdown X seconds, then return CountdownComplete. The length of the countdown
is determined by the module (likely in its config)
-}
countdown ::
   ModuleT s IO ()
countdown =
   undefined
