{-# LANGUAGE ExistentialQuantification #-}

module PostOffice (
	PostOffice,
	newPostOffice,
	getMailbox,
	createMailbox,
	destroyMailbox
) where

import Actor
import Mailbox
import Data.Map (Map)
import Data.Dynamic
import qualified Data.Map as Map
import Control.Concurrent.STM
import Control.Monad


type MailboxMap = Map ActorId Dynamic


newtype PostOffice = PostOffice (TVar MailboxMap)



newPostOffice :: IO PostOffice
newPostOffice = atomically create
	where create = PostOffice `liftM` (newTVar Map.empty)


getMailbox :: PostOffice -> (Actor a) -> IO (Maybe (Mailbox a))
getMailbox (PostOffice vm) a = atomically $ do
		m <- readTVar vm
		r <- return $ Map.lookup (actorId a) m
		--return (unwrap `fmap` r) 
		return Nothing
--undefined

createMailbox :: PostOffice -> (Actor a) -> IO (Mailbox a)
createMailbox = undefined

destroyMailbox :: PostOffice -> (Actor a) -> IO ()
destroyMailbox = undefined


{-
wrap :: (Typeable a) => (Mailbox a) -> Dynamic
wrap = toDyn

unwrap :: (Typeable a) => Dynamic -> (Mailbox a)
unwrap d = case fromDynamic d of
	(Just mb) -> mb
	Nothing   -> error "Typing error in PostOffice"
-}