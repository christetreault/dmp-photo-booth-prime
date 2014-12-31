{-|
Module : Photobooth Loop
Description : Photobooth Core Loop
Copyright : (c) Chris Tetreault, 2014

License : GPL-3
Stability : experimental
-}

module DMP.Photobooth.Loop
       (photoboothLoop)
       where

import Control.Concurrent.STM
import DMP.Photobooth.Core.Types
import DMP.Photobooth.Monads
import DMP.Photobooth.Core
import DMP.Photobooth.Module
import Control.Monad.Trans (liftIO)
import Data.ByteString.Lazy as BS

{-|
Photostrip processing loop.
1) wait for the trigger to be triggered
2) capture images
3) process a photostrip
4) print result
5) goto 1)
-}
photoboothLoop ::
   TVar Bool -- ^ If True, then the photostrip loop should exit
   -> CoreMonad cas ins pes phs prs trs ()
photoboothLoop shouldDie =
   do
      maybeSuccess <-
         listenOrDie shouldDie >>
         captureOrDie shouldDie  >>=
         processOrDie shouldDie >>=
         printOrDie shouldDie
      case maybeSuccess of
         Nothing -> return ()
         Just _ -> photoboothLoop shouldDie

listenOrDie ::
   TVar Bool
   -> CoreMonad cas ins pes phs prs trs (Maybe ())
listenOrDie shouldDie =
   undefined
   --(unwrap listen getTriggerStorage) >>= (waitOrDie shouldDie)

captureOrDie ::
   TVar Bool
   -> CoreMonad cas ins pes phs prs trs (Maybe [BS.ByteString])
captureOrDie shouldDie =
   undefined
   --timedCapture >>= (waitOrDie shouldDie)

processOrDie ::
   TVar Bool
   -> Maybe [BS.ByteString]
   -> CoreMonad cas ins pes phs prs trs (Maybe BS.ByteString)
processOrDie shouldDie (Just psl) =
   undefined
processOrDie _ Nothing =
   return Nothing
   --getPhotostripStorage >>= (process psl) >>= (waitOrDie shouldDie)

printOrDie ::
   TVar Bool
   -> Maybe BS.ByteString
   -> CoreMonad cas ins pes phs prs trs (Maybe ())
printOrDie shouldDie (Just strip) =
   undefined
printOrDie _ Nothing =
   return Nothing
   --getPrinterStorage >>= (startPrint strip) >>= (waitOrDie shouldDie)


{-|
Reads the result tvar. If the result is nothing, reads the shouldDie tvar. If
shouldDie is True, then returns Nothing. If shouldDie is false, then it
retries
-}
waitOrDie ::
   TVar Bool -- ^ If the function should give up
   -> TVar (Maybe a) -- ^ a value to try to read
   -> CoreMonad cas ins pes phs prs trs (Maybe a)
waitOrDie shouldDie result =
   liftIO $
   atomically $
   orElse
   (
      do
         r' <-
            readTVar result
         case r' of
            Nothing -> retry
            Just res -> return $ Just res)
   (
      do
         shouldDie' <-
            readTVar shouldDie
         if
            shouldDie'
            then
            return Nothing
            else
            retry)
