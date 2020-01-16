{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MonoLocalBinds         #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE NamedFieldPuns         #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeApplications       #-}
module Language.Plutus.Contract.StateMachine(
    -- $statemachine
    StateMachineClient(..)
    , TxConstraints
    , SMContractError(..)
    , AsSMContractError(..)
    , SM.StateMachine(..)
    , SM.StateMachineInstance(..)
    -- * Constructing the machine instance
    , SM.mkValidator
    , SM.mkStateMachine
    -- * Constructing the state machine client
    , mkStateMachineClient
    , defaultChooser
    -- * Running the state machine
    , runStep
    , runInitialise
    ) where

import           Control.Lens
import           Control.Monad.Error.Lens
import           Data.Text                         (Text)
import qualified Data.Text                         as Text

import           Language.Plutus.Contract
import qualified Language.Plutus.Contract.Typed.Tx as Tx
import qualified Language.PlutusTx                 as PlutusTx
import           Language.PlutusTx.StateMachine    (StateMachine (..), StateMachineInstance (..))
import qualified Language.PlutusTx.StateMachine    as SM
import           Ledger                            (Value)
import           Ledger.Constraints                (TxConstraints (..))
import           Ledger.Typed.Constraints          (TypedOrUntypedTxOuts (..), TypedTxConstraints, addTypedTxIn,
                                                    toTypedTxConstraints, toUntypedLedgerConstraints)
import           Ledger.Typed.Tx                   (TypedScriptTxOut (..))
import qualified Ledger.Typed.Tx                   as Typed
import           Ledger.Typed.TypeUtils            (hfHead)

import qualified Wallet.Typed.StateMachine         as SM

-- $statemachine
-- To write your contract as a state machine you need
-- * Two types @state@ and @input@ for the state and inputs of the machine
-- * A 'SM.StateMachineInstance state input' describing the transitions and
--   checks of the state machine (this is the on-chain code)
-- * A 'StateMachineClient state input' with the state machine instance and
--   an allocation function
--
-- In many cases it is enough to define the transition function
-- @t :: state -> input -> Value -> Maybe (PendingTxConstraints state)@ and use
-- 'mkStateMachine' and 'mkStateMachineClient' to get the client.
-- You can then use 'runInitialise' and 'runStep' to initialise and transition
-- the state machine. 'runStep' gets the current state from the utxo set and
-- makes the transition to the next state using the given input and taking care
-- of all payments.

data SMContractError s i =
    SMError (SM.SMError s i)
    | InvalidTransition s i
    | NonZeroValueAllocatedInFinalState
    | ChooserError Text
    deriving (Show)

makeClassyPrisms ''SMContractError

-- | Client-side definition of a state machine.
data StateMachineClient s i = StateMachineClient
    { scInstance :: SM.StateMachineInstance s i
    -- ^ The instance of the state machine, defining the machine's transitions,
    --   its final states and its check function.
    , scChooser  :: [SM.OnChainState s i] -> Either (SMContractError s i) (SM.OnChainState s i)
    -- ^ A function that chooses the relevant on-chain state, given a list of
    --   all potential on-chain states found at the contract address.
    }

-- | A state chooser function that fails if confronted with anything other
--   than exactly one output
defaultChooser ::
    forall state input
    . [SM.OnChainState state input]
    -> Either (SMContractError state input) (SM.OnChainState state input)
defaultChooser [x] = Right x
defaultChooser xs  =
    let msg = "Found " <> show (length xs) <> " outputs, expected 1"
    in Left (ChooserError (Text.pack msg))

-- | A state machine client with the 'defaultChooser' function
mkStateMachineClient ::
    forall state input
    . SM.StateMachineInstance state input
    -> StateMachineClient state input
mkStateMachineClient inst =
    StateMachineClient
        { scInstance = inst
        , scChooser  = defaultChooser
        }

getOnChainState :: (AsSMContractError e state i, PlutusTx.IsData state, HasUtxoAt schema) => StateMachineClient state i -> Contract schema e (SM.OnChainState state i)
getOnChainState StateMachineClient{scInstance, scChooser} = do
    utxo <- utxoAt (SM.machineAddress scInstance)
    let states = SM.getStates scInstance utxo
    either (throwing _SMContractError) pure $ scChooser states

-- | Run one step of a state machine, returning the new state.
runStep ::
    forall e state schema input.
    ( AsSMContractError e state input
    , AsContractError e
    , PlutusTx.IsData state
    , PlutusTx.IsData input
    , HasUtxoAt schema
    , HasWriteTx schema
    , HasTxConfirmation schema
    )
    => StateMachineClient state input
    -- ^ The state machine
    -> input
    -- ^ The input to apply to the state machine
    -> Contract schema e (Maybe state)
runStep smc input = do
    typedConstraints <- mkStep smc input
    case typedConstraints of
        Left c -> do -- no continuing output (final state)
            submitTxConfirmed (toUntypedLedgerConstraints c)
            pure Nothing
        Right c -> do -- has continuing output (nonfinal state)
            submitTxConfirmed (toUntypedLedgerConstraints c)
            let TypedOrUntypedTxOuts{totTypedTxOuts} = tcOutputs c
            pure $ Just $ tyTxOutData $ hfHead totTypedTxOuts

-- | Initialise a state machine
runInitialise ::
    forall e state schema input.
    ( PlutusTx.IsData state
    , AsContractError e
    , HasTxConfirmation schema
    , HasWriteTx schema
    )
    => StateMachineClient state input
    -- ^ The state machine
    -> state
    -- ^ The initial state
    -> Value
    -- ^ The value locked by the contract at the beginning
    -> Contract schema e state
runInitialise StateMachineClient{scInstance} initialState initialValue = do
    let StateMachineInstance{validatorInstance} = scInstance
        tx = Tx.makeScriptPayment validatorInstance initialValue initialState
    submitTxConfirmed (toUntypedLedgerConstraints tx)
    pure initialState

type StateMachineTypedTx state input =
    Either
        (TypedTxConstraints '[SM.StateMachine state input] '[]) -- halting
        (TypedTxConstraints '[SM.StateMachine state input] '[SM.StateMachine state input]) -- stepping

mkStep ::
    forall e state schema input.
    ( AsSMContractError e state input
    , HasUtxoAt schema
    , PlutusTx.IsData state
    , PlutusTx.IsData input
    )
    => StateMachineClient state input
    -> input
    -> Contract schema e (StateMachineTypedTx state input)
mkStep client@StateMachineClient{scInstance} input = do
    let StateMachineInstance{stateMachine=StateMachine{smTransition}, validatorInstance} = scInstance
    (TypedScriptTxOut{tyTxOutData=currentState}, txOutRef) <- getOnChainState client

    let typedTxIn = Typed.makeTypedScriptTxIn @(SM.StateMachine state input) validatorInstance input txOutRef
        balance = Typed.txInValue typedTxIn
    typedTxConstraints <- case smTransition currentState input balance >>= toTypedTxConstraints validatorInstance of
        Just s  -> pure s
        Nothing -> throwing _InvalidTransition (currentState, input)

    let txWithIns = bimap (addTypedTxIn typedTxIn) (addTypedTxIn typedTxIn) typedTxConstraints
    pure txWithIns
