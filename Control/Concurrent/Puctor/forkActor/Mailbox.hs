{-# LANGUAGE DeriveDataTypeable #-}

module Control.Concurrent.Puctor.ForkActor.Mailbox (
	Mailbox,
	newMailbox,
	enqueue,
	dequeue
) where

import Control.Concurrent
import Control.Concurrent.Chan
import Control.Monad

newtype Mailbox a = Mailbox (Chan a)

newMailbox :: IO (Mailbox a)
newMailbox = Mailbox `liftM` newChan

dequeue :: Mailbox a -> IO a
dequeue (Mailbox c) = readChan c

enqueue :: Mailbox a -> a -> IO ()
enqueue (Mailbox c) = writeChan c
