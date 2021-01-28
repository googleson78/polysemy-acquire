{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}

module Polysemy.AcquireSpec (spec) where

import Control.Concurrent.MVar (newMVar, putMVar, takeMVar, tryTakeMVar)
import Control.Exception (Exception, handle, throwIO)
import Control.Monad (void)
import Data.Acquire (ReleaseType (..), mkAcquire, mkAcquireType)
import Data.Maybe (isJust, isNothing)
import Data.Typeable (Typeable)
import Polysemy.Acquire (with)
import Polysemy.Error (catch, errorToIOFinal, runError, throw)
import Polysemy.Final (embedFinal, runFinal)
import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)

spec :: Spec
spec = describe "with" $ do
  it "calls both acquire and release" $ do
    x <- newMVar ()
    let xAcquire = mkAcquire (takeMVar x) (putMVar x)
    isAcquired <- fmap isNothing $ runFinal $ with xAcquire $ \_ -> embedFinal $ tryTakeMVar x
    isReleased <- isJust <$> tryTakeMVar x
    [isAcquired, isReleased] `shouldSatisfy` and

  it "calls release with ReleaseException in the case of an IO exception in the run function" $ do
    x <- newMVar ReleaseEarly
    let xAcquire = mkAcquireType (takeMVar x) (\_ releaseType -> putMVar x releaseType)
    handle (\Exc -> pure ()) $ runFinal $ with xAcquire $ \_ -> embedFinal $ throwIO Exc
    releaseType <- tryTakeMVar x
    releaseType `shouldBe` Just ReleaseException

  it "calls release with ReleaseException in the case of a pure Polysemy error" $ do
    x <- newMVar ReleaseEarly
    let xAcquire = mkAcquireType (takeMVar x) (\_ releaseType -> putMVar x releaseType)
    void $ runFinal $ runError $ with xAcquire $ \_ -> throw "bla"
    releaseType <- tryTakeMVar x
    releaseType `shouldBe` Just ReleaseException

  it "calls release with ReleaseNormal when a pure Polysemy error is caught" $ do
    x <- newMVar ReleaseEarly
    let xAcquire = mkAcquireType (takeMVar x) (\_ releaseType -> putMVar x releaseType)
    void $ runFinal $ runError $ with xAcquire $ \_ -> throw "bla" `catch` \_ -> pure "bla"
    releaseType <- tryTakeMVar x
    releaseType `shouldBe` Just ReleaseNormal

  it "calls release with ReleaseException in the case of a IO Polysemy error" $ do
    x <- newMVar ReleaseEarly
    let xAcquire = mkAcquireType (takeMVar x) (\_ releaseType -> putMVar x releaseType)
    void $ runFinal $ errorToIOFinal $ with xAcquire $ \_ -> throw "bla"
    releaseType <- tryTakeMVar x
    releaseType `shouldBe` Just ReleaseException

  it "calls release with ReleaseNormal when a IO Polysemy error is caught" $ do
    x <- newMVar ReleaseEarly
    let xAcquire = mkAcquireType (takeMVar x) (\_ releaseType -> putMVar x releaseType)
    void $ runFinal $ errorToIOFinal $ with xAcquire $ \_ -> throw "bla" `catch` \_ -> pure "bla"
    releaseType <- tryTakeMVar x
    releaseType `shouldBe` Just ReleaseNormal

data Exc = Exc
  deriving stock (Show, Typeable)
  deriving anyclass (Exception)
