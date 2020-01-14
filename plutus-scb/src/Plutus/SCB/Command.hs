{-# LANGUAGE NamedFieldPuns #-}

module Plutus.SCB.Command
    ( saveTxAggregate
    , saveRequestResponseAggregate
    ) where

import           Data.Maybe                 (catMaybes)
import           Eventful                   (Aggregate (Aggregate), aggregateCommandHandler, aggregateProjection)
import           Plutus.SCB.Events.Contract (ContractRequest, ContractResponse)
import qualified Plutus.SCB.Events.Contract as Contract
import           Plutus.SCB.Mocks           (ChainEvent (..), Tx (Tx), entries)
import           Plutus.SCB.Query           (nullProjection)

saveTxAggregate :: Aggregate () ChainEvent Tx
saveTxAggregate =
    Aggregate
        { aggregateProjection = nullProjection
        , aggregateCommandHandler = \() Tx {entries} -> RecordEntry <$> entries
        }

saveRequestResponseAggregate ::
       Aggregate () ChainEvent ( Contract.RequestEvent ContractRequest
                               , Maybe (Contract.RequestEvent ContractRequest)
                               , Maybe (Contract.ResponseEvent ContractResponse))
saveRequestResponseAggregate =
    Aggregate {aggregateProjection = nullProjection, aggregateCommandHandler}
  where
    aggregateCommandHandler _ (request, mCancellation, mResponse) =
        catMaybes
            [ Just $ RecordRequest request
            , RecordRequest <$> mCancellation
            , RecordResponse <$> mResponse
            ]
