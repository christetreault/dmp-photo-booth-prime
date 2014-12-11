{-|
Module : Photobooth
Description : Photobooth Core
Copyright : (c) Chris Tetreault, 2014

License : GPL-3
Stability : experimental
-}

module DMP.Photobooth where

import DMP.Photobooth.Core
import DMP.Photobooth.Core.Types
import DMP.Photobooth.Module.Types
import Control.Monad.Trans (liftIO)
import DMP.Photobooth.Module
import DMP.Photobooth.Monads
import Control.Concurrent.STM
import Control.Monad.Error
import Control.Concurrent
import Control.Monad.Trans.Maybe
import DMP.Photobooth.Loop


photoboothMain ::
   CoreMonad cas ins pes phs prs trs ()
photoboothMain =
   do
      initModules

      loopShouldDie <-
         liftIO $
            atomically $
               newTVar False

      -- launch interface

      finalizeModules



{-|
   Initializes all the modules
-}
initModules ::
   CoreMonad cas ins pes phs prs trs ()
initModules =
   do
      unwrap initPersistence getPersistenceStorage

      readAllConfigs

      unwrap initCamera getCameraStorage
      unwrap initInterface getInterfaceStorage
      unwrap initPhotostrip getPhotostripStorage
      unwrap initPrinter getPrinterStorage
      unwrap initTrigger getTriggerStorage

      return ()

{-|
   Finalizes all the modules
-}
finalizeModules ::
   CoreMonad cas ins pes phs prs trs ()
finalizeModules =
   do
      unwrap finalizeCamera getCameraStorage
      unwrap finalizePrinter getPrinterStorage
      unwrap finalizeTrigger getTriggerStorage
      unwrap finalizePhotostrip getPhotostripStorage
      unwrap finalizeInterface getInterfaceStorage
      unwrap finalizePersistence getPersistenceStorage

      return ()
