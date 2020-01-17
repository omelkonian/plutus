{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE ViewPatterns      #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-strictness #-}
{-# OPTIONS_GHC -fno-ignore-interface-pragmas #-}
{-# OPTIONS -fplugin-opt Language.PlutusTx.Plugin:debug-context #-}
-- | A guessing game that
--
--   * Uses a state machine to keep track of the current secret word
--   * Uses a token to keep track of who is allowed to make a guess
--

module Language.PlutusTx.Coordination.Contracts.GameStateMachine(
    contract
    , scriptInstance
    , GameToken
    , gameToken
    , gameTokenVal
    , mkValidator
    , LockArgs(..)
    , GuessArgs(..)
    , GameStateMachineSchema
    ) where

import Control.Lens (makeClassyPrisms)
import           Control.Monad                (void)
import qualified Language.PlutusTx            as PlutusTx
import           Language.PlutusTx.Prelude    hiding (check, Applicative (..))
import           Ledger                       hiding (to)
import qualified Ledger.Value                 as V
import qualified Ledger.Typed.Scripts         as Scripts

import qualified Data.ByteString.Lazy.Char8   as C

import           Language.Plutus.Contract.StateMachine (AsSMContractError)
import qualified Language.Plutus.Contract.StateMachine as SM

import           Language.Plutus.Contract

newtype HashedString = HashedString ByteString
    deriving newtype (PlutusTx.IsData, Eq)
    deriving stock Show

PlutusTx.makeLift ''HashedString

newtype ClearString = ClearString ByteString
    deriving newtype (PlutusTx.IsData, Eq)
    deriving stock Show

PlutusTx.makeLift ''ClearString

-- | Arguments for the @"lock"@ endpoint
data LockArgs =
    LockArgs
        { lockArgsSecret :: String
        -- ^ The secret
        , lockArgsValue  :: Value
        -- ^ Value that is locked by the contract initially
        } deriving Show

-- | Arguments for the @"guess"@ endpoint
data GuessArgs =
    GuessArgs
        { guessArgsOldSecret :: String
        -- ^ The guess
        , guessArgsNewSecret :: String
        -- ^ The new secret
        , guessArgsValueTakenOut :: Value
        -- ^ How much to extract from the contract
        } deriving Show

-- | The schema of the contract. It consists of the usual
--   'BlockchainActions' plus the two endpoints @"lock"@
--   and @"guess"@ with their respective argument types.
type GameStateMachineSchema =
    BlockchainActions
        .\/ Endpoint "lock" LockArgs
        .\/ Endpoint "guess" GuessArgs

data GameError =
    GameContractError ContractError
    | GameStateMachineError (SM.SMContractError GameState GameInput)
    deriving (Show)

-- | Top-level contract, exposing both endpoints.
contract :: Contract GameStateMachineSchema GameError ()
contract = (lock <|> guess) >> contract

-- | The token that represents the right to make a guess
newtype GameToken = GameToken { unGameToken :: Value }
    deriving newtype (Eq, Show)

-- | State of the guessing game
data GameState =
    Initialised HashedString
    -- ^ Initial state. In this state only the 'ForgeTokens' action is allowed.
    | Locked GameToken HashedString
    -- ^ Funds have been locked. In this state only the 'Guess' action is
    --   allowed.
    deriving (Show)

instance Eq GameState where
    {-# INLINABLE (==) #-}
    (Initialised s) == (Initialised s') = s == s'
    (Locked tn s) == (Locked tn' s') = s == s' && tn == tn'
    _ == _ = traceIfFalseH "states not equal" False

-- | Check whether a 'ClearString' is the preimage of a
--   'HashedString'
checkGuess :: HashedString -> ClearString -> Bool
checkGuess (HashedString actual) (ClearString gss) = actual == (sha2_256 gss)

-- | Inputs (actions)
data GameInput =
      ForgeToken GameToken
    -- ^ Forge the "guess" token
    | Guess ClearString HashedString Value
    -- ^ Make a guess, extract the funds, and lock the remaining funds using a
    --   new secret word.
    deriving (Show)

{-# INLINABLE transition #-}
transition :: (GameState, Value) -> GameInput -> Maybe (PendingTxConstraints GameState)
transition (state, currentVal) input = case (state, input) of
    (Initialised s, ForgeToken tk) -> 
        Just $ payToOwnAddress currentVal (Locked tk s)
                <> forgeValue (unGameToken tk)
    (Locked tn currentSecret, Guess theGuess nextSecret takenOut)
        | checkGuess currentSecret theGuess ->
        Just $ payToOwnAddress (currentVal - takenOut) (Locked tn nextSecret)
                <> moveValue (unGameToken tn)
    _ -> Nothing

{-# INLINABLE machine #-}
machine :: SM.StateMachine GameState GameInput
machine = SM.mkStateMachine transition

{-# INLINABLE mkValidator #-}
mkValidator :: Scripts.ValidatorType (SM.StateMachine GameState GameInput)
mkValidator = SM.mkValidator machine

scriptInstance :: Scripts.ScriptInstance (SM.StateMachine GameState GameInput)
scriptInstance = Scripts.validator @(SM.StateMachine GameState GameInput)
    $$(PlutusTx.compile [|| mkValidator ||])
    $$(PlutusTx.compile [|| wrap ||])
    where
        wrap = Scripts.wrapValidator @GameState @GameInput

-- | The 'SM.StateMachineInstance' of the game state machine contract. It uses
--   the functions in 'Language.PlutusTx.StateMachine' to generate a validator
--   script based on the functions 'step' and 'check' we defined above.
machineInstance :: SM.StateMachineInstance GameState GameInput
machineInstance = SM.StateMachineInstance machine scriptInstance

client :: SM.StateMachineClient GameState GameInput
client = SM.mkStateMachineClient machineInstance

gameToken :: GameToken
gameToken = GameToken (V.singleton curHash tokenName 1) where
    curHash = scriptCurrencySymbol (Scripts.validatorScript scriptInstance)
    tokenName = "guess"

gameTokenVal :: Value
gameTokenVal = unGameToken gameToken

-- | The @"guess"@ endpoint.
guess ::
    ( AsContractError e
    , AsSMContractError e GameState GameInput
    )
    => Contract GameStateMachineSchema e ()
guess = do
    GuessArgs{guessArgsOldSecret,guessArgsNewSecret, guessArgsValueTakenOut} <- endpoint @"guess"

    let guessedSecret = ClearString (C.pack guessArgsOldSecret)
        newSecret     = HashedString (sha2_256 (C.pack guessArgsNewSecret))

    void (SM.runStep client (Guess guessedSecret newSecret guessArgsValueTakenOut))

lock ::
    ( AsContractError e
    , AsSMContractError e GameState GameInput
    )
    => Contract GameStateMachineSchema e ()
lock = do
    LockArgs{lockArgsSecret, lockArgsValue} <- endpoint @"lock"
    let secret = HashedString (sha2_256 (C.pack lockArgsSecret))
    _ <- SM.runInitialise client (Initialised secret) lockArgsValue
    void (SM.runStep client (ForgeToken gameToken))

PlutusTx.makeIsData ''GameState
PlutusTx.makeLift ''GameState
PlutusTx.makeIsData ''GameInput
PlutusTx.makeLift ''GameInput
makeClassyPrisms ''GameError
PlutusTx.makeLift ''GameToken
PlutusTx.makeIsData ''GameToken

instance AsContractError GameError where
    _ContractError = _GameContractError

instance AsSMContractError GameError GameState GameInput where
    _SMContractError = _GameStateMachineError
