{-# LANGUAGE NoMonomorphismRestriction #-}

module ActorTest (
) where

import Control.Concurrent.Puctor.Actor
import Control.Concurrent.Puctor.Pure as P
import Control.Concurrent.Puctor.Pure.ActorState
import Control.Concurrent.Puctor.IO as I
import Control.Concurrent.Puctor.ForkActor as F
import Control.Concurrent.Puctor.ForkActorIO as FIO

import System.IO.Unsafe
import Control.Concurrent



greet :: (Actor a, Actor to) => P.Behaviour a ((to String, String))
greet = runActor greetStep
    where greetStep (a, name) = do
            a `send` ("Hi " ++ name)
            return Loop

test = runActor step
    where step nr = do
            a <- spawn $ FIO.newActor echo
            a `send` nr
            return Loop


handle :: Actor a => P.Behaviour a String
handle _ af msg = unsafePerformIO $ do 
    i <- myThreadId
    print $ msg ++ " [" ++ (show i) ++ "]"
    return $ P.Continue handle af []


echo = I.loop step
    where step a _ msg = do
            print $ "Actor " ++ (show $ actorId a) ++ " got: " ++ msg
            return []


main :: Actor a => P.Behaviour a ()
main = runActor $ \msg -> do
    e <- spawn $ FIO.newActor echo
    e `send` "One"
    --g <- spawn $ F.newActor test
    --g `send` "Two"
    h <- spawn $ F.newActor greet
    h `send` (e, "Mario")
    return End


start = do
    print "Start"
    boot $ F.newActor main
    threadDelay 1000
    print "End"
