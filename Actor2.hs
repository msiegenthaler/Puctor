{-# LANGUAGE ExistentialQuantification #-}

module Actor2 (
) where

import Data.Unique.Id
import Control.Monad.State


data Actor a = Actor Id

instance Show (Actor a) where
    show (Actor id) = "Actor " ++ (show id)
instance Eq (Actor a) where
    (Actor a) == (Actor b) = a == b


data Message a = Message { to :: (Actor a)
                         , msg :: a }

data Effect = forall a. Send (Message a)
            | forall a. Spawn Id (Behaviour a)
type Effects = [Effect]

data Next a = Terminate Effects
            | Continue (Behaviour a) Effects

type Behaviour a = a -> Next a


data ActorAcc a = ActorAcc { myself :: (Actor a)
                           , effects :: [Effect] --reverse
                           , idSupply :: IdSupply
                           }

type ActorState a r = State (ActorAcc a) r


self :: ActorState a (Actor a)
self = myself `liftM` get

spawn :: Behaviour a -> ActorState b (Actor a)
spawn b = do
        s <- get
        (a, s') <- return $ make s
        put s'
        return $ a
    where make s = (Actor id, s { idSupply = ids', effects = p' })
            where (id, ids') = newid $ idSupply s
                  p' = (Spawn id b) : effects s
newid s = (idFromSupply a, b)
    where (a, b) = splitIdSupply s


send :: (Actor a) -> a -> ActorState b ()
send a msg = do
        s <- get
        put $ perform s
    where perform s = s { effects = es' }
            where es' = (Send $ Message a msg) : effects s 


stop :: Behaviour a
stop _ = Terminate []

loop :: (a -> Effects) -> Behaviour a
loop f m = Continue (loop f) $ f m

once :: (a -> Effects) -> Behaviour a
once f m = Terminate $ f m


runActor :: (Next a) -> IO ()
runActor (Terminate es) = handleEffect `mapM` (reverse es) >> return ()
runActor (Continue b es) = handleEffect `mapM` (reverse es) >> run
    where run = return () --TODO wait for next..

handleEffect :: Effect -> IO ()
handleEffect (Send msg) = handleMessage msg
handleEffect (Spawn i b) = addActor i b

handleMessage :: (Message a) -> IO ()
handleMessage = undefined

addActor :: Id -> (Behaviour a) -> IO ()
addActor = undefined








