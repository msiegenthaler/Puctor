{-# LANGUAGE ExistentialQuantification #-}

module Actor (
  Actor,
  ActorId,
  ActorFactory,
  actorId,
  newActor,
  actorFactory,
  Message(..),
  Effect(..),
  Effects,
  Next(..),
  Behaviour
) where

import Mailbox
import Data.Unique.Id
import Control.Monad.State

newtype ActorId = ActorId Id deriving (Eq, Ord)
instance Show ActorId where
  show (ActorId id) = "<" ++ show id ++ ">"
    where num = tail $ show id

newtype ActorFactory = ActorFactory IdSupply

data Actor a = Actor ActorId

instance Show (Actor a) where
    show (Actor id) = "Actor " ++ (show id)
instance Eq (Actor a) where
    (Actor a) == (Actor b) = a == b


data Message a = Message { to :: (Actor a)
                         , msg :: a }

data Effect = forall a. Send (Message a)
            | forall a. Spawn (Actor a) (Behaviour a)
type Effects = [Effect]


data Next a = Terminate Effects
            | Continue (Behaviour a) ActorFactory Effects
type Behaviour a = (Actor a) -> ActorFactory -> a -> Next a


actorId :: (Actor a) -> ActorId
actorId (Actor aid) = aid

newActor :: ActorFactory -> (Actor a, ActorFactory)
newActor (ActorFactory ids) = (Actor id, ActorFactory ids')
  where (myIds, ids') = splitIdSupply ids
        id = ActorId $ idFromSupply myIds
  
actorFactory :: IO ActorFactory
actorFactory = ActorFactory `liftM` initIdSupply 'A'

