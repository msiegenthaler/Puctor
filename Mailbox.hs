module Mailbox (
	Mailbox,
	newMailbox,
	enqueue,
	dequeue
) where

import Control.Concurrent.STM
import Control.Concurrent
import Control.Monad


newtype Mailbox a = Mailbox (TChan a)

newMailbox :: IO (Mailbox a)
newMailbox = atomically $ Mailbox `liftM` newTChan

dequeue :: (Mailbox a) -> (IO a)
dequeue (Mailbox c) = atomically $ readTChan c

enqueue :: (Mailbox a) -> a -> (IO ())
enqueue (Mailbox c) msg = atomically $ writeTChan c msg
