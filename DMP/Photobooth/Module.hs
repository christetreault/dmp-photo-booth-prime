{-|
Module : Module
Description : Module Loader
Copyright : (c) Chris Tetreault, 2014

License : GPL-3
Stability : experimental

This module loads all interchangable modules.

The following modules are considered to be interchangable:
   Printer Module
   Trigger Module
   Camera Module
   Interface Module
   Photostrip Module
   Persistence Module

This module exposes all module functions to the core. However, the actual
implementations of the functions exported by this module may vary. The idea
here is that a developer can swap out an implemenation of any of these modules
with a new one, then compile with the new module. So long as the new module
implements the interface of its module, it will work seamlessly with the
program.

This file is a wrapper around the module implementations. The actual
implementations should be imported by this file. This file contains functions
that are, by default, undefined. These functions should be mapped to module
functions by defining the glue functions in this module.

The plan is to have a utility that generates this file at compile time.
However, this should be easy enough to do by hand.

-}

module DMP.Photobooth.Module where

import DMP.Photobooth.Module.Types
import DMP.Photobooth.Monads

-------------------------------------------------------------------------------
-- Import module implementations here -----------------------------------------
-------------------------------------------------------------------------------

import qualified DMP.Photobooth.Module.Printer as PrinterMod
import qualified DMP.Photobooth.Module.Trigger as TriggerMod
import qualified DMP.Photobooth.Module.Camera as CameraMod
import qualified DMP.Photobooth.Module.Interface as InterfaceMod
import qualified DMP.Photobooth.Module.Photostrip as PhotostripMod
import qualified DMP.Photobooth.Module.Persistence as PersistenceMod


-------------------------------------------------------------------------------
-- End of module imports ------------------------------------------------------
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- Printer module function definitions ----------------------------------------
-------------------------------------------------------------------------------

initPrinter ms =
   runModuleT
   (PrinterResult)
   ms
   PrinterMod.init

finalizePrinter ms =
   runModuleT
   (PrinterResult)
   ms
   PrinterMod.finalize

defaultPrinterConfig =
   PrinterMod.defaultConfig

initialPrinterState =
   PrinterMod.initialState

startPrint b ms =
   asyncCall
   (PrinterResult)
   ms $
   PrinterMod.printStrip
   b



-------------------------------------------------------------------------------
-- Trigger module function definitions ----------------------------------------
-------------------------------------------------------------------------------

listen ms =
   asyncCall
   (TriggerResult)
   ms $
   TriggerMod.listen

countdown ms =
   asyncCall
   (TriggerResult)
   ms $
   TriggerMod.countdown

initTrigger ms =
   runModuleT
   (TriggerResult)
   ms
   TriggerMod.init

finalizeTrigger ms =
   runModuleT
   (TriggerResult)
   ms
   TriggerMod.finalize

initialTriggerState =
   TriggerMod.initialState

defaultTriggerConfig =
   TriggerMod.defaultConfig

-------------------------------------------------------------------------------
-- Camera module function definitions -----------------------------------------
-------------------------------------------------------------------------------

initCamera ms =
   runModuleT
   (CameraResult)
   ms
   CameraMod.init

finalizeCamera ms =
   runModuleT
   (CameraResult)
   ms
   CameraMod.finalize

initialCameraState =
   CameraMod.initialState

defaultCameraConfig =
   CameraMod.defaultConfig

startCapture ms =
   asyncCall
   (CameraResult)
   ms
   CameraMod.capture

-------------------------------------------------------------------------------
-- Interface module function definitions --------------------------------------
-------------------------------------------------------------------------------

initInterface ms =
   runModuleT
   (InterfaceResult)
   ms
   InterfaceMod.init

finalizeInterface ms =
   runModuleT
   (InterfaceResult)
   ms
   InterfaceMod.finalize

initialInterfaceState =
   InterfaceMod.initialState

defaultInterfaceConfig =
   InterfaceMod.defaultConfig

-------------------------------------------------------------------------------
-- Photostrip module function definitions -------------------------------------
-------------------------------------------------------------------------------

process r ms =
   asyncCall
   (PhotostripResult)
   ms $
   PhotostripMod.process
   r

initPhotostrip ms =
   runModuleT
   (PhotostripResult)
   ms
   PhotostripMod.init

finalizePhotostrip ms =
   runModuleT
   (PhotostripResult)
   ms
   PhotostripMod.finalize

initialPhotostripState =
   PhotostripMod.initialState

defaultPhotostripConfig =
   PhotostripMod.defaultConfig

-------------------------------------------------------------------------------
-- Persistence module function definitions ------------------------------------
-------------------------------------------------------------------------------

persist m k p ms =
   runModuleT
   (PersistenceResult)
   ms $
   PersistenceMod.persist
   m
   k
   p

restore m k ms =
   runModuleT
   (PersistenceResult)
   ms $
   PersistenceMod.restore
   m
   k

initPersistence ms =
   runModuleT
   (PersistenceResult)
   ms
   PersistenceMod.init

finalizePersistence ms =
   runModuleT
   (PersistenceResult)
   ms
   PersistenceMod.finalize

initialPersistenceState =
   PersistenceMod.initialState

defaultPersistenceConfig =
   PersistenceMod.defaultConfig
