{-|
Module : Core
Description : Photobooth Core
Copyright : (c) Chris Tetreault, 2014

License : GPL-3
Stability : experimental
-}

module DMP.Photobooth.Core where

import DMP.Photobooth.Module
import DMP.Photobooth.Module.Types
import DMP.Photobooth.Core.Types
import DMP.Photobooth.Monads
import Control.Monad
import Control.Monad.Trans
import Control.Concurrent.STM
import Control.Monad.State


-- | Key for the camera module configs
cameraConfigKey = "cameraConfig"

-- | Key for the interface module configs
interfaceConfigKey = "interfaceConfig"

-- | Key for the persistence module configs
persistenceConfigKey = "persistenceConfig"

-- | Key for the photostrip module configs
photostripConfigKey = "photostripConfig"

-- | Key for the printer module configs
printerConfigKey = "printerConfig"

-- | Key for the trigger module configs
triggerConfigKey = "triggerConfig"

{-|
Reads in all module configs from the persistence module. If restore throws,
this function fails. If restore returns Nothing, then the config remains
the default value. If restore returns a result, then the config is updated
-}
readAllConfigs ::
   CoreMonad cas ins pes phs prs trs ()
readAllConfigs =
   do
      readModConfig
         Persistence
         persistenceConfigKey
         (getPersistenceStorage)
         (setPersistenceStorage)
      readModConfig
         Camera
         cameraConfigKey
         (getCameraStorage)
         (setCameraStorage)
      readModConfig
         Interface
         interfaceConfigKey
         (getInterfaceStorage)
         (setInterfaceStorage)
      readModConfig
         Photostrip
         photostripConfigKey
         (getPhotostripStorage)
         (setPhotostripStorage)
      readModConfig
         Printer
         printerConfigKey
         (getPrinterStorage)
         (setPrinterStorage)
      readModConfig
         Trigger
         triggerConfigKey
         (getTriggerStorage)
         (setTriggerStorage)

{-|
Reads in a module config from the persistence module. If restore throws,
this function fails. If restore returns Nothing, then the config remains
the default value. If restore returns a result, then the config is updated
-}
readModConfig ::
   Source -- ^ The module substore to query
   -> String -- ^ The key to lookup
   -> (CoreMonad cas ins pes phs prs trs (ModuleStorage a)) -- ^ The
                  -- get___Storage function corresponding to this config
   -> (ModuleStorage a
          -> CoreMonad cas ins pes phs prs trs ()) -- ^ The set___Storage
                  -- corresponding to this config
   -> CoreMonad cas ins pes phs prs trs ()
readModConfig s k gfn sfn =
   do
      conf <-
         unwrap
            (restore
                s
                k)
            getPersistenceStorage
      modS <- gfn
      updateConfig
         (sfn)
         modS
         s
         conf
   where
      updateConfig _ _ _ Nothing =
         return ()
      updateConfig fn s mn (Just p) =
         do
            fn
               s
                  {modConfig =
                      Configuration
                         {moduleName = mn,
                          moduleConfig = p}}
            return ()

{-|
Pushes a LogEntry to the Log Queue
-}
pushLog ::
   LogEntry
   -> CoreMonad cas ins pes phs prs trs ()
pushLog l =
   do
      cs <- get
      liftIO $
         atomically $
            writeTQueue (csLogQueue cs) l

{-|
Pushes all logs from this result to the Log Queue
-}
pushLogs ::
   Result cas ins pes phs prs trs r
   -> CoreMonad cas ins pes phs prs trs ()
pushLogs r =
   do
      logs <-
         return $
            resultLog r
      sequence_ $ map pushLog logs

{-|
Stores the state in a Result into the Core state
-}
keepState ::
   Result cas ins pes phs prs trs r
   -> CoreMonad cas ins pes phs prs trs ()
keepState (CameraResult  _ s _) =
   do
      store <- getCameraStorage
      setCameraStorage $
         store {modState = s}
keepState (InterfaceResult  _ s _) =
   do
      store <- getInterfaceStorage
      setInterfaceStorage $
         store {modState = s}
keepState (PersistenceResult  _ s _) =
   do
      store <- getPersistenceStorage
      setPersistenceStorage $
         store {modState = s}
keepState (PhotostripResult  _ s _) =
   do
      store <- getPhotostripStorage
      setPhotostripStorage $
         store {modState = s}
keepState (PrinterResult  _ s _) =
   do
      store <- getPrinterStorage
      setPrinterStorage $
         store {modState = s}
keepState (TriggerResult  _ s _) =
   do
      store <- getTriggerStorage
      setTriggerStorage $
         store {modState = s}

{-|
Runs the passed-in computation, keeps state and pushes logs before
returning the result
-}
unwrap ::
   (ModuleStorage s
       -> IO (Result cas ins pes phs prs trs r)) -- ^ The computation to
                -- perform. Should be partially applied with all arguments
                -- except the ModuleStorage
   -> CoreMonad cas ins pes phs prs trs (ModuleStorage s)
   -> CoreMonad cas ins pes phs prs trs r
unwrap mfn ms =
   do
      modStore <- ms
      runit <-
         liftIO $ mfn modStore
      unwrapResult runit


unwrapResult ::
   Result cas ins pes phs prs trs r
   -> CoreMonad cas ins pes phs prs trs r
unwrapResult r =
   do
      keepState r
      pushLogs r
      pushIfError $
         resultValue r
      return $
         fromEither $ resultValue r
   where fromEither (Right r') = r'

{-|
If an error is encountered, pushes it, then fails the core monad
computation
-}
pushIfError ::
   Either LogEntry r
   -> CoreMonad cas ins pes phs prs trs ()
pushIfError (Left e) =
   do
      pushLog e
      mzero
pushIfError _ =
   return ()

{-|
Builds the initial state of the Core
-}
initCoreState ::
   IO (CoreState cas ins pes phs prs trs)
initCoreState =
   do
      logQueue <-
         atomically newTQueue
      return $
         CoreState
            {csLogQueue =
               logQueue,
             csCamera =
               ModuleStorage
                  {modState = initialCameraState,
                   modConfig =
                     Configuration
                        {moduleName = Camera,
                         moduleConfig = defaultCameraConfig}},
             csInterface =
                ModuleStorage
                   {modState = initialInterfaceState,
                    modConfig =
                     Configuration
                        {moduleName = Interface,
                         moduleConfig = defaultInterfaceConfig}},
             csPersistence =
                ModuleStorage
                   {modState = initialPersistenceState,
                    modConfig =
                     Configuration
                        {moduleName = Persistence,
                         moduleConfig = defaultPersistenceConfig}},
             csPhotostrip =
                ModuleStorage
                   {modState = initialPhotostripState,
                    modConfig =
                     Configuration
                        {moduleName = Photostrip,
                         moduleConfig = defaultPhotostripConfig}},
             csPrinter =
                ModuleStorage
                   {modState = initialPrinterState,
                    modConfig =
                     Configuration
                        {moduleName = Printer,
                         moduleConfig = defaultPrinterConfig}},
             csTrigger =
                ModuleStorage
                   {modState = initialTriggerState,
                    modConfig =
                     Configuration
                        {moduleName = Trigger,
                         moduleConfig = defaultTriggerConfig}}}
