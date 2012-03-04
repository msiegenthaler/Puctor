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



actorName = show . actorId

greet :: (Actor a, Actor to) => P.Behaviour a ((to String, String))
greet = runActor step
    where step (a, name) = do
            i <- self
            a `send` ("Hi " ++ name ++ " from " ++ (actorName i))
            return Loop

test = runActor step
    where step nr = do
            a <- spawn $ FIO.newActor echo
            a `send` nr
            return Loop


echo = I.loop step
    where step a _ msg = do
            print $ "Actor " ++ (show $ actorId a) ++ " got: " ++ msg
            return []

noop = runActor step
    where step _ = do
            return Loop


main :: Actor a => P.Behaviour a ()
main = runActor $ \msg -> do
    e <- spawn $ FIO.newActor echo
    e `send` "One"
    h <- spawn $ F.newActor greet
    h `send` (e, "Mario")
    e `send` "Two"
    return End


start = do
    print "Start"
    boot $ F.newActor main
    threadDelay 300000
    print "End"