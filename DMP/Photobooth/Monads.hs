{-|
Module : Monads
Description : Photobooth Monads
Copyright : (c) Chris Tetreault, 2014

License : GPL-3
Stability : experimental
-}

module DMP.Photobooth.Monads where

import DMP.Photobooth.Module.Types
import DMP.Photobooth.Core.Types
import Control.Monad.State
import Control.Concurrent.STM
import Control.Monad.Trans.Maybe
import Control.Monad.Error
import Control.Concurrent
import Control.Monad.Writer
import Control.Monad.Trans
import Control.Monad.Reader

-- | Executes the core monad. Used as the entry point for the program
runCoreMonad ::
   CoreState cas ins pes phs prs trs -- ^ Should be the initial state
   -> CoreMonad cas ins pes phs prs trs a
   -> IO (Maybe a)
runCoreMonad s m =
   evalStateT
      (runMaybeT m)
      s

{-|
   The photobooth core monad. Manages manages the configs of all modules and
   facilitates transactions between modules.
-}
type CoreMonad cas ins pes phs prs trs a =
   MaybeT
   (StateT (CoreState cas ins pes phs prs trs)
   IO) a

-- | get the ModuleStorage associated with the Camera module
getCameraStorage ::
   CoreMonad cas ins pes phs prs trs (ModuleStorage cas)
getCameraStorage =
   gets csCamera

-- | get the ModuleStorage associated with the Interface module
getInterfaceStorage ::
   CoreMonad cas ins pes phs prs trs (ModuleStorage ins)
getInterfaceStorage =
   gets csInterface

-- | get the ModuleStorage associated with the Persistence module
getPersistenceStorage ::
   CoreMonad cas ins pes phs prs trs (ModuleStorage pes)
getPersistenceStorage =
   gets csPersistence

-- | get the ModuleStorage associated with the Photostrip module
getPhotostripStorage ::
   CoreMonad cas ins pes phs prs trs (ModuleStorage phs)
getPhotostripStorage =
   gets csPhotostrip

-- | get the ModuleStorage associated with the Printer module
getPrinterStorage ::
   CoreMonad cas ins pes phs prs trs (ModuleStorage prs)
getPrinterStorage =
   gets csPrinter

-- | get the ModuleStorage associated with the Trigger module
getTriggerStorage ::
   CoreMonad cas ins pes phs prs trs (ModuleStorage trs)
getTriggerStorage =
   gets csTrigger

-- | update the ModuleStorage associated with the Camera module
setCameraStorage ::
   ModuleStorage cas
   -> CoreMonad cas ins pes phs prs trs ()
setCameraStorage s =
   do
      state <- get
      put $
         state {csCamera = s}

-- | update the ModuleStorage associated with the Interface module
setInterfaceStorage ::
   ModuleStorage ins
   -> CoreMonad cas ins pes phs prs trs ()
setInterfaceStorage s =
   do
      state <- get
      put $
         state {csInterface = s}

-- | update the ModuleStorage associated with the Persistence module
setPersistenceStorage ::
   ModuleStorage pes
   -> CoreMonad cas ins pes phs prs trs ()
setPersistenceStorage s =
   do
      state <- get
      put $
         state {csPersistence = s}

-- | update the ModuleStorage associated with the Photostrip module
setPhotostripStorage ::
   ModuleStorage phs
   -> CoreMonad cas ins pes phs prs trs ()
setPhotostripStorage s =
   do
      state <- get
      put $
         state {csPhotostrip = s}

-- | update the ModuleStorage associated with the Printer module
setPrinterStorage ::
   ModuleStorage prs
   -> CoreMonad cas ins pes phs prs trs ()
setPrinterStorage s =
   do
      state <- get
      put $
         state {csPrinter = s}

-- | update the ModuleStorage associated with the Trigger module
setTriggerStorage ::
   ModuleStorage trs
   -> CoreMonad cas ins pes phs prs trs ()
setTriggerStorage s =
   do
      state <- get
      put $
         state {csTrigger = s}

{-|
   A minimal monad transfomer that modules are expected to operate within. 
   Provides logging, configuration, and state management.
-}
type ModuleT s m a = 
   ErrorT LogEntry
   (WriterT [LogEntry]
   (ReaderT Configuration
   (StateT (Maybe (TVar s)) m))) a

{-|
   Execute a ModuleT, returning the log, and the value of the passed-in monad
   wrapped in the inner monad
-}
runModuleT ::
   (Monad m)
   => ([LogEntry] 
          -> (Maybe (TVar s)) 
          -> (Either LogEntry a)
          -> (Result cas ins pes phs prs trs a))
   -> ModuleStorage s
   -> ModuleT s m a
   -> m (Result cas ins pes phs prs trs a)
runModuleT ctor moduleStorage moduleMonad =
   do
      result <-
         runStateT 
            (runReaderT 
                (runWriterT
                    (runErrorT moduleMonad))
                (modConfig moduleStorage))
            (modState moduleStorage)
      return $
         ctor
            (snd $ fst result)
            (snd result)
            (fst $ fst result)

{-|
   Perform a ModuleT computation asynchronously. Returns a TVar that will
   eventually contain the Result
-}
asyncCall ::
   ([LogEntry] 
       -> (Maybe (TVar s)) 
       -> (Either LogEntry a)
       -> (Result cas ins pes phs prs trs a))
   -> ModuleStorage s
   -> ModuleT s IO a
   -> CoreMonad cas ins pes phs prs trs
         (TVar
            (Maybe 
               (Result cas ins pes phs prs trs a)
            )
         ) -- ^ A TVar, containing Just (a result) if
           -- the thread finished, Nothing if it is still running
asyncCall ctor moduleStorage moduleMonad =
   do
      resultTVar <-
         liftIO $
            atomically $
               newTVar Nothing
      liftIO $
         forkIO $ 
            do
               result <-
                  runModuleT
                     ctor
                     moduleStorage
                     moduleMonad
               atomically $
                  writeTVar
                     resultTVar
                     (Just result)
      return resultTVar

{-|
   Log a message into the Module monad
-}
logit ::
   (Monad m)
   => LogSeverity -- ^ The severity of the message
   -> String -- ^ The text of the message
   -> ModuleT s m ()
logit sev msg =
   do
      src <- whoAmI
      tell [(LogEntry src sev msg)]
      return ()

{-|
   Fail from a ModuleT computation, returning a final error message
-}
logFail ::
   (Monad m)
   => String -- ^ The text of the message
   -> ModuleT s m ()
logFail msg =
   do
      src <- whoAmI
      throwError $
         LogEntry
         src
         Fatal
         msg

{-|
   Queries the configuration to see what the current module is
-}
whoAmI ::
   (Monad m)
   => ModuleT s m Source
whoAmI =
   do
      conf <- ask
      return (moduleName conf)

{-|
   Returns the configuration of this module, minus metadata
-}
askConfig ::
   (Monad m)
   => ModuleT s m Persistable
askConfig =
   do
      conf <- ask
      return (moduleConfig conf)
