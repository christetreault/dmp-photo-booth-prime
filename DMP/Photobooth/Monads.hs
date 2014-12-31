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
      st <- get
      put $
         st {csCamera = s}

-- | update the ModuleStorage associated with the Interface module
setInterfaceStorage ::
   ModuleStorage ins
   -> CoreMonad cas ins pes phs prs trs ()
setInterfaceStorage s =
   do
      st <- get
      put $
         st {csInterface = s}

-- | update the ModuleStorage associated with the Persistence module
setPersistenceStorage ::
   ModuleStorage pes
   -> CoreMonad cas ins pes phs prs trs ()
setPersistenceStorage s =
   do
      st <- get
      put $
         st {csPersistence = s}

-- | update the ModuleStorage associated with the Photostrip module
setPhotostripStorage ::
   ModuleStorage phs
   -> CoreMonad cas ins pes phs prs trs ()
setPhotostripStorage s =
   do
      st <- get
      put $
         st {csPhotostrip = s}

-- | update the ModuleStorage associated with the Printer module
setPrinterStorage ::
   ModuleStorage prs
   -> CoreMonad cas ins pes phs prs trs ()
setPrinterStorage s =
   do
      st <- get
      put $
         st {csPrinter = s}

-- | update the ModuleStorage associated with the Trigger module
setTriggerStorage ::
   ModuleStorage trs
   -> CoreMonad cas ins pes phs prs trs ()
setTriggerStorage s =
   do
      st <- get
      put $
         st {csTrigger = s}

{-|
A minimal monad transfomer that modules are expected to operate within.
Provides logging, configuration, and state management.
-}
type ModuleT s a =
   ErrorT LogEntry
   (WriterT [LogEntry]
   (ReaderT Configuration
   (StateT (Maybe (TVar s)) IO))) a

{-|
Execute a ModuleT, returning the log, and the value of the passed-in monad
wrapped in the inner monad
-}
runModuleT ::
   ([LogEntry]
       -> (Maybe (TVar s))
       -> (Either LogEntry a)
       -> (Result cas ins pes phs prs trs a))
   -> ModuleStorage s
   -> ModuleT s a
   -> IO (Result cas ins pes phs prs trs a)
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
   -> ModuleT s a
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
   LogSeverity -- ^ The severity of the message
   -> String -- ^ The text of the message
   -> ModuleT s ()
logit sev msg =
   do
      src <- whoAmI
      tell [(LogEntry src sev msg)]
      return ()

{-|
Fail from a ModuleT computation, returning a final error message
-}
logFail ::
   String -- ^ The text of the message
   -> ModuleT s ()
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
   ModuleT s Source
whoAmI =
   do
      conf <- ask
      return (moduleName conf)

{-|
Returns the configuration of this module, minus metadata
-}
askConfig ::
   ModuleT s Persistable
askConfig =
   do
      conf <- ask
      return (moduleConfig conf)
