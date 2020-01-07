{-# LANGUAGE MonoLocalBinds    #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE ViewPatterns      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-ignore-interface-pragmas #-}
{-# OPTIONS_GHC -fno-omit-interface-pragmas #-}
{-# OPTIONS_GHC -fno-strictness #-}
{-# OPTIONS_GHC -fno-specialise #-}
-- | A multisig contract written as a state machine.
--   $multisig
module Language.PlutusTx.Coordination.Contracts.MultiSigStateMachine(
      Params(..)
    , Payment(..)
    , State
    , mkValidator
    , scriptInstance
    , MultiSigError(..)
    , MultiSigSchema
    , contract
    ) where

import           Control.Lens                 (makeClassyPrisms)
import qualified Ledger.Interval              as Interval
import           Ledger.Validation            (PendingTx, PendingTx'(..))
import qualified Ledger.Validation            as Validation
import qualified Ledger.Typed.Scripts         as Scripts
import           Ledger.Value                 (Value)
import qualified Ledger.Value                 as Value
import           Ledger                       (PubKey, Slot)

import qualified Language.PlutusTx            as PlutusTx
import           Language.Plutus.Contract
import           Language.Plutus.Contract.StateMachine (AsSMContractError, StateMachine(..))
import qualified Language.Plutus.Contract.StateMachine as SM
import           Language.PlutusTx.Prelude         hiding (Applicative (..))

--   $multisig
--   The n-out-of-m multisig contract works like a joint account of
--   m people, requiring the consent of n people for any payments.
--   In the smart contract the signatories are represented by public keys,
--   and approval takes the form of signatures on transactions.
--
--   The multisig contract in
--   'Language.PlutusTx.Coordination.Contracts.MultiSig' expects n signatures on
--   a single transaction. This requires an off-chain communication channel. The
--   multisig contract implemented in this module uses a proposal system that
--   allows participants to propose payments and attach their signatures to
--   proposals over a period of time, using separate transactions. All contract
--   state is kept on the chain so there is no need for off-chain communication.

-- | A proposal for making a payment under the multisig scheme.
data Payment = Payment
    { paymentAmount    :: Value
    -- ^ How much to pay out
    , paymentRecipient :: PubKey
    -- ^ Address to pay the value to
    , paymentDeadline  :: Slot
    -- ^ Time until the required amount of signatures has to be collected.
    }
    deriving (Show)

instance Eq Payment where
    {-# INLINABLE (==) #-}
    (Payment vl pk sl) == (Payment vl' pk' sl') = vl == vl' && pk == pk' && sl == sl'


data Params = Params
    { mspSignatories  :: [PubKey]
    -- ^ Public keys that are allowed to authorise payments
    , mspRequiredSigs :: Integer
    -- ^ How many signatures are required for a payment
    }

-- | State of the multisig contract.
data State =
    Holding
    -- ^ Money is locked, anyone can make a proposal for a payment. If there is
    -- no value here then this is a final state and the machine will terminate.

    | CollectingSignatures Payment [PubKey]
    -- ^ A payment has been proposed and is awaiting signatures.
    deriving (Show)

instance Eq State where
    {-# INLINABLE (==) #-}
    Holding == Holding = True
    (CollectingSignatures pmt pks) == (CollectingSignatures pmt' pks') =
        pmt == pmt' && pks == pks'
    _ == _ = False

data Input =
    ProposePayment Payment
    -- ^ Propose a payment. The payment can be made as soon as enough
    --   signatures have been collected.

    | AddSignature PubKey
    -- ^ Add a signature to the sigs. that have been collected for the
    --   current proposal.

    | Cancel
    -- ^ Cancel the current proposal if the deadline has passed

    | Pay
    -- ^ Make the payment.
    deriving Show

data MultiSigError =
    MSContractError ContractError
    | MSStateMachineError (SM.SMContractError State Input)
    deriving Show
makeClassyPrisms ''MultiSigError

instance AsContractError MultiSigError where
    _ContractError = _MSContractError

instance AsSMContractError MultiSigError State Input where
    _SMContractError = _MSStateMachineError

type MultiSigSchema =
    BlockchainActions
        .\/ Endpoint "propose-payment" Payment
        .\/ Endpoint "add-signature" ()
        .\/ Endpoint "cancel-payment" ()
        .\/ Endpoint "pay" ()
        .\/ Endpoint "lock" Value

{-# INLINABLE isSignatory #-}
-- | Check if a public key is one of the signatories of the multisig contract.
isSignatory :: PubKey -> Params -> Bool
isSignatory pk (Params sigs _) = any (\pk' -> pk == pk') sigs

{-# INLINABLE containsPk #-}
-- | Check whether a list of public keys contains a given key.
containsPk :: PubKey -> [PubKey] -> Bool
containsPk pk = any (\pk' -> pk' == pk)

{-# INLINABLE isValidProposal #-}
-- | Check whether a proposed 'Payment' is valid given the total
--   amount of funds currently locked in the contract.
isValidProposal :: Value -> Payment -> Bool
isValidProposal vl (Payment amt _ _) = amt `Value.leq` vl

{-# INLINABLE proposalExpired #-}
-- | Check whether a proposed 'Payment' has expired.
proposalExpired :: PendingTx -> Payment -> Bool
proposalExpired PendingTx{pendingTxValidRange} Payment{paymentDeadline} =
    paymentDeadline `Interval.before` pendingTxValidRange

{-# INLINABLE proposalAccepted #-}
-- | Check whether enough signatories (represented as a list of public keys)
--   have signed a proposed payment.
proposalAccepted :: Params -> [PubKey] -> Bool
proposalAccepted (Params signatories numReq) pks =
    let numSigned = length (filter (\pk -> containsPk pk pks) signatories)
    in numSigned >= numReq

{-# INLINABLE valuePreserved #-}
-- | @valuePreserved v p@ is true if the pending transaction @p@ pays the amount
--   @v@ to this script's address. It does not assert the number of such outputs:
--   this is handled in the generic state machine validator.
valuePreserved :: Value -> PendingTx -> Bool
valuePreserved vl ptx = vl == Validation.valueLockedBy ptx (Validation.ownHash ptx)

{-# INLINABLE valuePaid #-}
-- | @valuePaid pm ptx@ is true if the pending transaction @ptx@ pays
--   the amount specified in @pm@ to the public key address specified in @pm@
valuePaid :: Payment -> PendingTx -> Bool
valuePaid (Payment vl pk _) ptx = vl == (Validation.valuePaidTo ptx pk)

{-# INLINABLE transition #-}
transition :: Params -> State -> Input -> Value -> Maybe (PendingTxConstraints State)
transition params s i currentValue = case (s, i) of
    (Holding, ProposePayment pmt)
        | isValidProposal currentValue pmt ->
            Just $ payToOwnAddress currentValue (CollectingSignatures pmt [])
    (CollectingSignatures pmt pks, AddSignature pk)
        | isSignatory pk params && not (containsPk pk pks) ->
            let newState = CollectingSignatures pmt (pk:pks)
            in Just $ payToOwnAddress currentValue newState
                <> signedBy pk
    (CollectingSignatures payment _, Cancel) ->
        Just $ payToOwnAddress currentValue Holding
                <> validIn (Interval.from (paymentDeadline payment))
    (CollectingSignatures payment pks, Pay)
        | proposalAccepted params pks ->
            let Payment{paymentAmount, paymentRecipient, paymentDeadline} = payment
                newValue = currentValue - paymentAmount
            in Just $ payToOwnAddress newValue Holding
                <> payToPubKey paymentAmount paymentRecipient
                <> validIn (Interval.to paymentDeadline)
    _ -> Nothing

{-# INLINABLE mkValidator #-}
mkValidator :: Params -> Scripts.ValidatorType MultiSigSym
mkValidator p = SM.mkValidator $ SM.mkStateMachine (transition p)

validatorCode :: Params -> PlutusTx.CompiledCode (Scripts.ValidatorType MultiSigSym)
validatorCode params = $$(PlutusTx.compile [|| mkValidator ||]) `PlutusTx.applyCode` PlutusTx.liftCode params

type MultiSigSym = StateMachine State Input

scriptInstance :: Params -> Scripts.ScriptInstance MultiSigSym
scriptInstance params = Scripts.validator @MultiSigSym
    (validatorCode params)
    $$(PlutusTx.compile [|| wrap ||])
    where
        wrap = Scripts.wrapValidator @State @Input

machineInstance :: Params -> SM.StateMachineInstance State Input
machineInstance params =
    SM.StateMachineInstance
    (SM.mkStateMachine (transition params))
    (scriptInstance params)

client :: Params -> SM.StateMachineClient State Input
client p = SM.mkStateMachineClient (machineInstance p)

contract ::
    ( AsContractError e
    , AsSMContractError e State Input
    )
    => Params
    -> Contract MultiSigSchema e ()
contract params = go where
    theClient = client params
    go = endpoints >> go
    endpoints = (Just <$> lock) <|> propose <|> cancel <|> addSignature <|> pay
    propose = endpoint @"propose-payment" >>= SM.runStep theClient . ProposePayment
    cancel  = endpoint @"cancel-payment" >> SM.runStep theClient Cancel
    addSignature = endpoint @"add-signature" >> ownPubKey >>= SM.runStep theClient . AddSignature
    lock = do
        value <- endpoint @"lock"
        SM.runInitialise theClient Holding value
    pay = endpoint @"pay" >> SM.runStep theClient Pay

PlutusTx.makeIsData ''Payment
PlutusTx.makeLift ''Payment
PlutusTx.makeIsData ''State
PlutusTx.makeLift ''State
PlutusTx.makeLift ''Params
PlutusTx.makeIsData ''Input
PlutusTx.makeLift ''Input
