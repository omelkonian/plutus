{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeFamilies        #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Plutus.SCB.Query
    ( balances
    , trialBalance
    , nullProjection
    , eventCount
    , requestStats
    , RequestStats
    ) where

import           Control.Lens                    (makeLenses, over)
import           Control.Monad                   ((>=>))
import           Data.Map.Strict                 (Map)
import qualified Data.Map.Strict                 as Map
import           Data.Maybe                      (fromMaybe)
import           Data.Set                        (Set)
import qualified Data.Set                        as Set
import           Eventful                        (Projection (Projection), StreamEvent (StreamEvent), StreamProjection,
                                                  VersionedStreamEvent, projectionEventHandler, projectionSeed,
                                                  streamProjectionState)
import           Ledger.Ada                      (lovelaceValueOf)
import           Ledger.Value                    (Value)
import           Options.Applicative.Help.Pretty (Pretty, indent, int, pretty, vsep, (<+>))
import           Plutus.SCB.Events.Contract      (EventId, RequestEvent (CancelRequest, IssueRequest),
                                                  ResponseEvent (ResponseEvent))
import           Plutus.SCB.Mocks                (AccountId, ChainEvent (..), Entry (Entry), accountId, amount)

nullProjection :: Projection () event
nullProjection =
    Projection {projectionSeed = (), projectionEventHandler = const}

balances :: Projection (Map AccountId Value) (VersionedStreamEvent ChainEvent)
balances = Projection {projectionSeed = mempty, projectionEventHandler}
  where
    projectionEventHandler acc (StreamEvent _ _ (RecordEntry Entry { accountId
                                                                   , amount
                                                                   })) =
        Map.alter updater accountId acc
      where
        updater :: Maybe Value -> Maybe Value
        updater current = Just $ amount <> fromMaybe (lovelaceValueOf 0) current
    projectionEventHandler acc _ = acc

trialBalance :: Projection Value (VersionedStreamEvent ChainEvent)
trialBalance = Projection {projectionSeed = mempty, projectionEventHandler}
  where
    projectionEventHandler total (StreamEvent _ _ (RecordEntry Entry {amount})) =
        total <> amount
    projectionEventHandler total _ = total

eventCount :: Projection Int (VersionedStreamEvent ChainEvent)
eventCount = Projection {projectionSeed = 0, projectionEventHandler}
  where
    projectionEventHandler count _ = count + 1

data RequestStats =
    RequestStats
        { _made      :: Int
        , _cancelled :: Int
        , _responded :: Int
        , _openIds   :: Set EventId
        }
    deriving (Show, Eq)

makeLenses ''RequestStats

requestStats :: Projection RequestStats (VersionedStreamEvent ChainEvent)
requestStats =
    Projection
        { projectionSeed = RequestStats 0 0 0 Set.empty
        , projectionEventHandler = trackEventIds >=> countMessageTypes
        }

countMessageTypes ::
       RequestStats -> VersionedStreamEvent ChainEvent -> RequestStats
countMessageTypes stats (StreamEvent _ _ event) =
    case event of
        RecordRequest (IssueRequest _ _)   -> over made (+ 1) stats
        RecordRequest (CancelRequest _)    -> over cancelled (+ 1) stats
        RecordResponse (ResponseEvent _ _) -> over responded (+ 1) stats
        _                                  -> stats

trackEventIds :: RequestStats -> VersionedStreamEvent ChainEvent -> RequestStats
trackEventIds stats (StreamEvent _ _ event) =
    case event of
        RecordRequest (IssueRequest eventId _) ->
            over openIds (Set.insert eventId) stats
        RecordRequest (CancelRequest eventId) ->
            over openIds (Set.delete eventId) stats
        RecordResponse (ResponseEvent eventId _) ->
            over openIds (Set.delete eventId) stats
        _ -> stats

instance Pretty RequestStats where
    pretty RequestStats {_made, _cancelled, _responded, _openIds} =
        vsep
            [ "Request Stats:"
            , indent 2 $
              vsep
                  [ "Made:" <+> int _made
                  , "Cancelled:" <+> int _cancelled
                  , "Responded:" <+> int _responded
                  , "Open:" <+> int (Set.size _openIds)
                  ]
            ]

instance Pretty state =>
         Pretty (StreamProjection key position state event) where
    pretty = pretty . streamProjectionState
