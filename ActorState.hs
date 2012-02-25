module ActorState (
) where

import Data.Unique.Id
import Control.Monad.State
import Actor3


data ActorAcc a = ActorAcc { myself :: (Actor a)
                           , effects :: [Effect]
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
