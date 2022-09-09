{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RecordWildCards            #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Contract.Contract where

import           Control.Monad        hiding (fmap)
import           Data.Aeson           (ToJSON, FromJSON)
import           Data.Map             as Map
import           Data.Text            (Text)
import           Data.Void            (Void)
import           Data.Default
import           GHC.Generics         (Generic)
import           PlutusTx             (Data (..))
import qualified PlutusTx
import           PlutusTx.Prelude     hiding (Semigroup(..), unless)
import           Ledger               hiding (singleton)
import           Ledger.Constraints   (TxConstraints)
import qualified Ledger.Constraints   as Constraints
import qualified Ledger.Typed.Scripts as Scripts
import           Ledger.Ada           as Ada
import           Ledger.Value         as Value
import           Ledger.TimeSlot
import           Ledger.Time          as Time
import           Ledger.Interval      as Interval
import           Plutus.Contract      as Contract
import           Playground.Contract  (printJson, printSchemas, ensureKnownCurrencies, stage, ToSchema)
import           Playground.TH        (mkKnownCurrencies, mkSchemaDefinitions)
import           Playground.Types     (KnownCurrency (..))
import           Prelude              (IO, Semigroup (..), Show (..), String)
import qualified Prelude              as P
import           Text.Printf          (printf)
import Plutus.Trace.Emulator  as Emulator
import Wallet.Emulator.Wallet

-- Datum for Book Training TX
data BookTrainingDatum = BookTrainingDatum
    { client          :: PaymentPubKeyHash
    , bookingId       :: Integer
    , trainer         :: PaymentPubKeyHash
    , cancelDeadline  :: POSIXTime
    , deposit         :: Integer
    } deriving (Show, Generic, FromJSON, ToJSON)

PlutusTx.unstableMakeIsData ''BookTrainingDatum

-- Redeemers for this Contract
data BookingAction = Cancel | Fullfill | Delay
    deriving P.Show

PlutusTx.unstableMakeIsData ''BookingAction

-- Validator for On-Chain validation
-- When validating a Cancel or Delay action the transaction must be signed by the client and and within the cancelDeadline of the Booking
-- When validating a Fullfill action the transaction must be signed by the trainer and the cancel deadline has to be passed
{-# INLINABLE mkValidator #-}
mkValidator :: BookTrainingDatum -> BookingAction -> ScriptContext -> Bool
mkValidator dat redeemer ctx = case redeemer of
  Cancel   -> traceIfFalse "client's signature missing" $ signedByClient  && traceIfFalse "deadline passed"  withinDeadlineRange
  Delay    -> traceIfFalse "client's signature missing" $ signedByClient  && traceIfFalse "deadline passed"  withinDeadlineRange
  Fullfill -> traceIfFalse "Fullfill Invalid"           $ signedByTrainer && traceIfFalse "within Deadline"  passedDeadline
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    signedByTrainer :: Bool
    signedByTrainer = txSignedBy info $ unPaymentPubKeyHash $ trainer dat

    signedByClient :: Bool
    signedByClient = txSignedBy info $ unPaymentPubKeyHash $ client dat

    passedDeadline :: Bool
    passedDeadline = contains (from $ cancelDeadline dat) $ txInfoValidRange info

    withinDeadlineRange :: Bool
    withinDeadlineRange = overlaps (to $ cancelDeadline dat) $ txInfoValidRange info

-- Typeinstances for Datum and Redeemer of the BookTraining Contract
data BookTraining
instance Scripts.ValidatorTypes BookTraining where
    type instance DatumType BookTraining = BookTrainingDatum
    type instance RedeemerType BookTraining = BookingAction

typedValidator :: Scripts.TypedValidator BookTraining
typedValidator = Scripts.mkTypedValidator @BookTraining
    $$(PlutusTx.compile [|| mkValidator ||])
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.wrapValidator @BookTrainingDatum @BookingAction

--Validator Helpers
validator :: Validator
validator = Scripts.validatorScript typedValidator

valHash :: Ledger.ValidatorHash
valHash = Scripts.validatorHash typedValidator

scrAddress :: Ledger.Address
scrAddress = scriptAddress validator

-- Off-Chain helpers
--Deadline (dl) should not be reached
isCancable :: POSIXTime -> POSIXTime -> Bool
isCancable n dl = n <= dl

--Delaying is only allow during the first half of the deadline (dl) interval
isDelayable :: POSIXTime -> POSIXTime -> Bool
isDelayable n dl = n <= (dl - 20000)

--Fullfilling is only possible after the deadline (dl) interveral
isFullfilled :: POSIXTime -> POSIXTime -> Bool
isFullfilled n dl = n > dl


-- Off-Chain Code

--Params for Endpoint Book Training
data BookTrainingParams = BookTrainingParams
    { btTrainer  :: PaymentPubKeyHash
    , btBookingId  :: Integer
    } deriving (Generic, ToJSON, FromJSON, ToSchema)

--Params for Endpoint Cancel Training
data CancelTrainingParams = CancelTrainingParams
    {
      ctTrainer  :: !PaymentPubKeyHash
    , ctBookingId  :: !Integer
    } deriving (Generic, ToJSON, FromJSON, ToSchema)

--Params for Endpoint Delay Training
data DelayTrainingParams = DelayTrainingParams
    {
      dTrainer  :: !PaymentPubKeyHash
    , dBookingId  :: !Integer
    } deriving (Generic, ToJSON, FromJSON, ToSchema)

--Params for Endpoint Fullfill Training
data FullfillTrainingParams = FullfillTrainingParams
    {
      ffClient     :: !PaymentPubKeyHash
    , ffBookingId  :: !Integer
    } deriving (Generic, ToJSON, FromJSON, ToSchema)


type TrainerPayout = Integer
type Deposit       = Integer
type BookingSchema =
               Endpoint "bookTraining" BookTrainingParams
             .\/ Endpoint "cancelTraining" CancelTrainingParams
             .\/ Endpoint "fullfillTraining" FullfillTrainingParams
             .\/ Endpoint "delayTraining" DelayTrainingParams

-- Starts the contract and submits a transaction that locks 10_000_000 Lovelace and holds an BookTrainingDatum Record in the script
bookTraining :: AsContractError e => BookTrainingParams -> Contract w s e ()
bookTraining btp = do
    pkh      <- ownPaymentPubKeyHash
    now      <- currentTime
    let depositAmt = 10_000_000
        dat = BookTrainingDatum
                { client          = pkh
                , bookingId       = btBookingId btp
                , trainer         = btTrainer btp
                , cancelDeadline  = now + 40000
                , deposit         = depositAmt
                }
        tx  = Constraints.mustPayToTheScript dat $ Ada.lovelaceValueOf $ deposit dat
    ledgerTx <- submitTxConstraints typedValidator tx
    void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
    logInfo @String $ printf "made  %d lovelace deposit to Trainer %s with cancel deadline %s"
      (deposit dat)
      (show $ trainer dat)
      (show $ cancelDeadline dat)

cancelTraining :: forall w s. CancelTrainingParams -> Contract w s Text ()
cancelTraining ctp = do
    now   <- currentTime
    pkh   <- ownPaymentPubKeyHash
    logInfo @String $ printf "currentTime: %d , pkh: %s" (getPOSIXTime now) (show $ unPaymentPubKeyHash pkh)
    (oref, o, d) <- findBooking (ctBookingId ctp) (ctTrainer ctp)
    if isCancable now (cancelDeadline d)
      then do
          let remainingTime  = abs $ P.subtract now (cancelDeadline d)
              tPayout = trainerPayout remainingTime (deposit d)
              r       = Redeemer $ PlutusTx.toBuiltinData Cancel
              lookups = Constraints.typedValidatorLookups typedValidator <>
                        Constraints.otherScript validator                <>
                        Constraints.unspentOutputs (Map.singleton oref o)
              tx      = Constraints.mustPayToPubKey (ctTrainer ctp) (Ada.lovelaceValueOf tPayout) <>
                        Constraints.mustValidateIn (from now)                                     <>
                        Constraints.mustSpendScriptOutput oref r
          logInfo @String $ printf "tRemaining: %d" (getPOSIXTime remainingTime)
          ledgerTx <- submitTxConstraintsWith lookups tx
          void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
          logInfo @String $ "canceled booking"
      else
        logInfo @String $ "Booking not cancable"
      where
        trainerPayout :: POSIXTime -> Deposit -> TrainerPayout
        trainerPayout tRemaining deposit
            | tRemaining >= 30000 = (deposit * 25) `P.div` 100
            | tRemaining >= 20000 = (deposit * 50) `P.div` 100
            | tRemaining >= 10000 = (deposit * 75) `P.div` 100
            | otherwise           = (deposit * 75) `P.div` 100


delayTraining :: forall w s. DelayTrainingParams -> Contract w s Text ()
delayTraining dtp = do
    now   <- currentTime
    pkh   <- ownPaymentPubKeyHash
    logInfo @String $ printf "currentTime: %d , pkh: %s" (getPOSIXTime now) (show $ unPaymentPubKeyHash pkh)
    (oref, o, d) <- findBooking (dBookingId dtp) (dTrainer dtp)
    if isDelayable now (cancelDeadline d)
      then do
          let remainingTime  = abs $ P.subtract now (cancelDeadline d)
              tPayout = trainerPayout (deposit d)
              deposit' = (deposit d) - tPayout
              dat'    = BookTrainingDatum
                          { client          = client d
                          , bookingId       = bookingId d
                          , trainer         = trainer d
                          , cancelDeadline  = (cancelDeadline d) + 40000
                          , deposit         = deposit'
                          }
              r       = Redeemer $ PlutusTx.toBuiltinData Delay
              lookups = Constraints.typedValidatorLookups typedValidator <>
                        Constraints.otherScript validator                <>
                        Constraints.unspentOutputs (Map.singleton oref o)
              tx      = Constraints.mustPayToPubKey (dTrainer dtp) (Ada.lovelaceValueOf tPayout) <>
                        Constraints.mustValidateIn (from now)                                    <>
                        Constraints.mustPayToTheScript (dat') (Ada.lovelaceValueOf deposit')     <>
                        Constraints.mustSpendScriptOutput oref r
          logInfo @String $ printf "tRemaining: %d" (getPOSIXTime remainingTime)
          ledgerTx <- submitTxConstraintsWith lookups tx
          void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
          logInfo @String $ "delayed booking"
      else
        logInfo @String $ "Booking not delayable"
      where
        trainerPayout :: Deposit -> TrainerPayout
        trainerPayout d = (d * 25) `P.div` 100


fullfillTraining :: forall w s. FullfillTrainingParams -> Contract w s Text ()
fullfillTraining fftp = do
  now   <- currentTime
  pkh   <- ownPaymentPubKeyHash
  (oref, o, d) <- findBooking (ffBookingId fftp) pkh
  if isFullfilled now (cancelDeadline d)
    then do
      let r       = Redeemer $ PlutusTx.toBuiltinData Fullfill
          lookups = Constraints.typedValidatorLookups typedValidator    <>
                    Constraints.otherScript validator                   <>
                    Constraints.unspentOutputs (Map.singleton oref o)
          tx      = Constraints.mustValidateIn (from now)               <>
                    Constraints.mustSpendScriptOutput oref r
      ledgerTx <- submitTxConstraintsWith lookups tx
      void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
      logInfo @String $ "Payout done"
    else
      logInfo @String $ "Training has not been fullfilled yet"

-- Finds a Booking UTXO by Trainer or Client PKH
findBooking :: Integer
            -> PaymentPubKeyHash
            -> Contract w s Text (TxOutRef, ChainIndexTxOut, BookTrainingDatum)
findBooking bId pkh = do
    utxos <- utxosAt scrAddress
    let xs = [ (oref, o)
             | (oref, o) <- Map.toList utxos
             ]
    case xs of
        [(oref, o)] -> case _ciTxOutDatum o of
            Left _          -> Contract.throwError "datum missing"
            Right (Datum e) -> case PlutusTx.fromBuiltinData e of
                Nothing -> Contract.throwError "datum has wrong type"
                Just d
                    | bookingId d == bId && (trainer d == pkh || client d == pkh) -> return (oref, o, d)
                    | otherwise                              -> Contract.throwError "Booking missmatch"
        _           -> Contract.throwError "Booking utxo not found"


endpoints :: Contract () BookingSchema Text ()
endpoints = awaitPromise (bookTraining' `select` cancelTraining' `select` fullfillTraining' `select` delayTraining') >> endpoints
  where
    bookTraining' = endpoint @"bookTraining" bookTraining
    cancelTraining' = endpoint @"cancelTraining" $ cancelTraining
    fullfillTraining' = endpoint @"fullfillTraining" $ fullfillTraining
    delayTraining' = endpoint @"delayTraining" $ delayTraining


mkSchemaDefinitions ''BookingSchema

$(mkKnownCurrencies [])
