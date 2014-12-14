{-|
Module : Types
Description : Module Datatypes
Copyright : (c) Chris Tetreault, 2014

License : GPL-3
Stability : experimental

The datatypes defined here represent the data structures that the core believes
represents the various modules. Modules may do whatever they please internally,
but they must use these types to represent themselves to the core.
-}

module DMP.Photobooth.Module.Types where

import Control.Concurrent.STM
import Control.Monad.Error

{-|
The result of a ModuleT computation
-}
data Result cas ins pes phs prs trs r =
   CameraResult
      {caResultLog :: [LogEntry],
       caResultState :: Maybe (TVar cas),
       caResultValue :: (Either LogEntry r)} |
   InterfaceResult
      {inResultLog :: [LogEntry],
       inResultState :: Maybe (TVar ins),
       inResultValue :: (Either LogEntry r)} |
   PersistenceResult
      {peResultLog :: [LogEntry],
       peResultState :: Maybe (TVar pes),
       peResultValue :: (Either LogEntry r)} |
   PhotostripResult
      {phResultLog :: [LogEntry],
       phResultState :: Maybe (TVar phs),
       phResultValue :: (Either LogEntry r)} |
   PrinterResult
      {prResultLog :: [LogEntry],
       prResultState :: Maybe (TVar prs),
       prResultValue :: (Either LogEntry r)} |
   TriggerResult
      {trResultLog :: [LogEntry],
       trResultState :: Maybe (TVar trs),
       trResultValue :: (Either LogEntry r)}

{-|
Helper function to extract the value from a Result
-}
resultValue ::
    Result cas ins pes phs prs trs r
    -> Either LogEntry r
resultValue (CameraResult _ _ v) = v
resultValue (InterfaceResult _ _ v) = v
resultValue (PersistenceResult _ _ v) = v
resultValue (PhotostripResult _ _ v) = v
resultValue (PrinterResult _ _ v) = v
resultValue (TriggerResult _ _ v) = v

{-|
Helper function to extract the value from a Result
-}
resultLog ::
    Result cas ins pes phs prs trs r
    -> [LogEntry]
resultLog (CameraResult l _ _) = l
resultLog (InterfaceResult l _ _) = l
resultLog (PersistenceResult l _ _) = l
resultLog (PhotostripResult l _ _) = l
resultLog (PrinterResult l _ _) = l
resultLog (TriggerResult l _ _) = l

{-|
A log entry. Formalizes the format of the log output.
-}
data LogEntry =
   LogEntry
      {logSource :: LogSource,
       logSeverity :: LogSeverity,
       logMessage :: String}

instance Show LogEntry where
   show e =
      (show $ logSource e) ++
      "  ::  " ++
      (show $ logSeverity e) ++
      "  ::  " ++
      logMessage e

instance Error LogEntry where
   noMsg = LogEntry Core Fatal "Module reports an unrecoverable error!"
   strMsg m = LogEntry Core Fatal m

{-|
When a module needs to identify itself to the core, it should use this type
-}
data Source =
   Core |
   Camera |
   Interface |
   Persistence |
   Photostrip |
   Printer |
   Trigger
   deriving (Show, Eq)

{-|
When logging a message within the Module monad, this type is used to specify
the originater of the log message
-}
type LogSource =
   Source

{-|
When logging a message within the Module monad, this type is used to specify
the severity of the log message.
-}
data LogSeverity =
   Debug |
   Note |
   Minor |
   Moderate |
   Severe |
   Critical |
   Fatal
   deriving (Show, Eq, Ord)

{-|
A List of persistable key-value pairs. This type is the type that the
persistence module is able to persist. A persistable can store integers,
strings, doubles, bools, or nested lists of properties. Any arbitrary type
that implements @Read@ and @Show@ can be persisted by way of serialization.
-}
type Persistable =
   Property

{-|
Datatype for a configuration. Contains a Persistable and metadata fields
-}
data Configuration =
   Configuration
      {moduleName :: Source,
       moduleConfig :: Persistable}

{-|
An individual persistable value
-}
data Property =
   IntProperty
      {propName :: String,
       intValue :: Integer} |
   StringProperty
      {propName :: String,
       stringValue :: String} |
   DoubleProperty
      {propName :: String,
       doubleValue :: Double} |
   BoolProperty
      {propName :: String,
       boolValue :: Bool} |
   ListProperty
      {propName :: String,
       listValue :: [Property]}
   deriving (Eq, Show)
