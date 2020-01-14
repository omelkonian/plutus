{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Plutus.SCB.CoreSpec
    ( tests
    ) where

import           Control.Monad.Trans.State      (evalState)
import           Data.Foldable                  (traverse_)
import           Eventful                       (UUID, allEvents, commandStoredAggregate, getEvents,
                                                 getLatestStreamProjection, globalStreamProjection,
                                                 streamProjectionState)
import           Eventful.Store.Memory          (emptyEventMap, stateEventStoreReader, stateEventStoreWriter,
                                                 stateGlobalEventStoreReader)
import           Ledger.Value                   (isZero)
import           Plutus.SCB.Command             (saveTxAggregate)
import           Plutus.SCB.Mocks               (Tx, entries)
import           Plutus.SCB.Query               (eventCount, trialBalance)
import           Test.QuickCheck.Instances.UUID ()
import           Test.Tasty                     (TestTree, testGroup)
import           Test.Tasty.QuickCheck          (conjoin, counterexample, property, testProperty, (===))

tests :: TestTree
tests = testGroup "SCB.Core" [trialBalanceTests]

trialBalanceTests :: TestTree
trialBalanceTests =
    testGroup
        "trialBalance"
        [ testProperty "Aggregate always creates 2 events" $ \uuid tx ->
              let (events, finalState) =
                      flip evalState emptyEventMap $ do
                          generatedEvents <-
                              commandStoredAggregate
                                  stateEventStoreWriter
                                  stateEventStoreReader
                                  saveTxAggregate
                                  uuid
                                  tx
                          storedEvents <-
                              getEvents
                                  stateGlobalEventStoreReader
                                  (allEvents ())
                          pure (generatedEvents, storedEvents)
               in conjoin [length events === 2, length finalState === 2]
        , testProperty "Stores the correct number of events" $ \(uuid :: UUID) (txs :: [Tx]) ->
              let (queryEvents, storedEvents) =
                      flip evalState emptyEventMap $ do
                          traverse_
                              (commandStoredAggregate
                                   stateEventStoreWriter
                                   stateEventStoreReader
                                   saveTxAggregate
                                   uuid)
                              txs
                          queryResult <-
                              getLatestStreamProjection
                                  stateGlobalEventStoreReader $
                              globalStreamProjection eventCount
                          storedResult <-
                              getEvents stateGlobalEventStoreReader $
                              allEvents ()
                          pure (queryResult, storedResult)
                  queryEventCount = streamProjectionState queryEvents
                  entryCount = length $ concatMap entries txs
                  storedEventCount = length storedEvents
               in conjoin
                      [ counterexample
                            "Queried events do not equal stored events" $
                        queryEventCount === storedEventCount
                      , counterexample
                            "Stored events do not equal generated events" $
                        storedEventCount === entryCount
                      ]
        , testProperty "Overall balance is always 0" $ \(uuid :: UUID) (txs :: [Tx]) ->
              let (queryEvents, storedEvents) =
                      flip evalState emptyEventMap $ do
                          traverse_
                              (commandStoredAggregate
                                   stateEventStoreWriter
                                   stateEventStoreReader
                                   saveTxAggregate
                                   uuid)
                              txs
                          queryResult <-
                              getLatestStreamProjection
                                  stateGlobalEventStoreReader $
                              globalStreamProjection trialBalance
                          storedResult <-
                              getEvents stateGlobalEventStoreReader $
                              allEvents ()
                          pure (queryResult, storedResult)
                  balance = streamProjectionState queryEvents
                  entryCount = length $ concatMap entries txs
                  storedEventCount = length storedEvents
               in counterexample "Accounts are unbalanced" $
                  property $ isZero balance
        ]
