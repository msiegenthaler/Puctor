{-# LANGUAGE DeriveDataTypeable #-}

module Control.Concurrent.Puctor.ForkActor.Mailbox (
	Mailbox,
	newMailbox,
	enqueue,
	dequeue
) where

import Control.Concurrent.STM
import Control.Concurrent
import Control.Monad
import Data.Dynamic


newtype Mailbox a = Mailbox (TChan a) deriving (Typeable)

newMailbox :: IO (Mailbox a)
newMailbox = Mailbox `liftM` newTChanIO

dequeue :: (Mailbox a) -> (IO a)
dequeue (Mailbox c) = atomically $ readTChan c

enqueue :: (Mailbox a) -> a -> (IO ())
enqueue (Mailbox c) msg = atomically $ writeTChan c msg
