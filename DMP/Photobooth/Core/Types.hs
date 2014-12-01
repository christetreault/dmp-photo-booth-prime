{-|
Module : Types
Description : Photobooth Core Types
Copyright : (c) Chris Tetreault, 2014

License : GPL-3
Stability : experimental
-}

module DMP.Photobooth.Core.Types where

import DMP.Photobooth.Module.Types
import Control.Monad.State
import Control.Concurrent.STM
import Control.Monad.Trans.Maybe

{-|
   An atomic, threadsafe, queue to push log entries into
-}
type LogQueue = TQueue LogEntry

{-| 
   The core stores the module configs and states between module monad
   invocations. This type is used to store this data
-}
data ModuleStorage a =
   ModuleStorage
      {modState :: Maybe (TVar a), -- ^ The state of a module. Since this is
             -- a threaded program, the states are protected using STM.
       modConfig :: Configuration  -- ^ The configuration of a module
       }

{-|
   The state of the Core. Stores all module configs and states 
-}
data CoreState cas ins pes phs prs trs =
   CoreState
      {csLogQueue :: LogQueue,
       csCamera :: ModuleStorage cas,
       csInterface :: ModuleStorage ins,
       csPersistence :: ModuleStorage pes,
       csPhotostrip :: ModuleStorage phs,
       csPrinter :: ModuleStorage prs,
       csTrigger :: ModuleStorage trs}
