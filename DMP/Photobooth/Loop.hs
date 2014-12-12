{-|
Module : Photobooth Loop
Description : Photobooth Core Loop
Copyright : (c) Chris Tetreault, 2014

License : GPL-3
Stability : experimental
-}

module DMP.Photobooth.Loop where

import Control.Concurrent.STM
import DMP.Photobooth.Core.Types
import DMP.Photobooth.Monads
import DMP.Photobooth.Core
import DMP.Photobooth.Module
import Control.Monad.Trans (liftIO)

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
      lr <-
         listenOrDie
      case lr of
         Nothing -> return ()
         Just msg -> undefined

      return ()
   where
      listenOrDie =
         getTriggerStorage >>= listen >>= waitOrDie shouldDie


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
   (do
       r' <-
          readTVar result
       case r' of
          Nothing -> retry
          Just res -> return $ Just res)
   (do
       shouldDie' <-
          readTVar shouldDie
       if
          shouldDie'
          then
          return Nothing
          else
          retry)
