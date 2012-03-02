{-# LANGUAGE ExistentialQuantification, FlexibleInstances #-}

module Control.Concurrent.Puctor.Actor (
  Actor,
  ActorId,
  ActorFactory,
  ActorCreator,
  actorId,
  (!),
  sendMsg,
  newActorId,
  splitActorFactory,
  Message(..),
  ActorSpawn(..),
  Effect(..),
  Effects,
  initialActor,
  boot
) where

import Data.Unique.Id
import Control.Monad.State


newtype ActorId = ActorId Id deriving (Eq, Ord)
instance Show ActorId where
  show (ActorId id) = "<" ++ show id ++ ">"
    where num = tail $ show id

class Actor actor where
    actorId :: actor msg -> ActorId
    (!) :: actor msg -> msg -> IO ()

{- TODO
instance (Actor a) => Show (a b) where
    show = ("Actor "++) . show . actorId
instance (Actor a) => Eq (a b) where
    a == b = (actorId a) == (actorId b)
-}

newtype ActorFactory = ActorFactory IdSupply

data Message actor msg = Message  { to :: actor msg
                                  , msg :: msg }

data ActorSpawn a = ActorSpawn { actor       :: a
                               , spawnAction :: IO ()}                            


data Effect = forall actor msg. Actor actor => Send (Message actor msg)
            | forall actor msg. Actor actor => Spawn (ActorSpawn (actor msg))
type Effects = [Effect]


type ActorCreator a = ActorFactory -> (ActorFactory, ActorSpawn a)

newActorId :: ActorFactory -> (ActorId, ActorFactory)
newActorId af = idFromActorFactory `mapfst` (splitActorFactory af)

mapfst f (a,b) = (f a, b)

idFromActorFactory :: ActorFactory -> ActorId
idFromActorFactory (ActorFactory ids) = ActorId $ idFromSupply ids

splitActorFactory :: ActorFactory -> (ActorFactory, ActorFactory)
splitActorFactory (ActorFactory ids) = (ActorFactory idsa, ActorFactory idsb)
    where (idsa, idsb) = splitIdSupply ids
  
actorFactory :: IO ActorFactory
actorFactory = ActorFactory `liftM` initIdSupply 'A'


sendMsg to msg = Send $ Message to msg

initialActor :: Actor a => ActorCreator (a msg) -> IO (a msg)
initialActor c = do
  s <- (snd . c) `liftM` actorFactory
  spawnAction s
  return (actor s)

boot :: Actor a => ActorCreator (a ()) -> IO ()
boot c = initialActor c >>= (flip (!)) () >> return ()
