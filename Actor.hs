{-# LANGUAGE ExistentialQuantification #-}

module Actor3 (
  Actor,
  ActorId,
  ActorFactory,
  newActor,
  actorFacory,
  Message(..),
  Effect(..),
  Effects,
  Next(..),
  Behaviour
) where

import Mailbox
import Data.Unique.Id
import Control.Monad.State

--TODO mailbox should just be a type class

newtype ActorId = ActorId Id deriving Eq
instance Show ActorId where
  show (ActorId id) = "<" ++ show id ++ ">"
    where num = tail $ show id

newtype ActorFactory = ActorFactory IdSupply

data Actor a = Actor ActorId (Mailbox a)

instance Show (Actor a) where
    show (Actor id _) = "Actor " ++ (show id)
instance Eq (Actor a) where
    (Actor a _) == (Actor b _) = a == b


data Message a = Message { to :: (Actor a)
                         , msg :: a }

data Effect = forall a. Send (Message a)
            | forall a. Spawn (Actor a) (Behaviour a)
type Effects = [Effect]


data Next a = Terminate Effects
            | Continue (Behaviour a) Effects
type Behaviour a = a -> Next a



newActor :: ActorFactory -> (Mailbox a) -> (Actor a, ActorFactory)
newActor (ActorFactory ids) mb = (Actor id mb, ActorFactory ids')
  where (myIds, ids') = splitIdSupply ids
        id = ActorId $ idFromSupply myIds
  
actorFacory :: IO ActorFactory
actorFacory = ActorFactory `liftM` initIdSupply 'A'

enqueueMessage :: (Actor a) -> a -> IO ()
enqueueMessage (Actor _ mb) = enqueue mb