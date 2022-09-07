{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds        #-}

module Contract.Trace where

import Control.Monad.Freer.Extras as Extras
import Data.Default               (Default (..))
import Data.Functor               (void)
import Ledger.TimeSlot
import Plutus.Trace
import Wallet.Emulator.Wallet
import Plutus.Trace.Emulator  as Emulator

import Contract.Contract

-- A trace that invokes the pay endpoint of payContract on Wallet 1 twice, each time with Wallet 2 as
-- recipient, but with amounts given by the two arguments. There should be a delay of one slot
-- after each endpoint call.

-- import Waller.Emulator
-- knownWallet 2
-- mockWalletPaymentPubKEyHash knownWallet 2
bookingTraceCancel :: EmulatorTrace ()
bookingTraceCancel = do
  h1 <- activateContractWallet (knownWallet 1) endpoints
  let pkh = mockWalletPaymentPubKeyHash $ knownWallet 2
  callEndpoint @"bookTraining" h1 $ BookTrainingParams {
            btTrainer = pkh
          , btBookingId = 10001
          }
  void $ Emulator.waitNSlots 35
  callEndpoint @"cancelTraining" h1 $ CancelTrainingParams
    { ctTrainer   = pkh
    , ctBookingId = 10001
    }
  void $ Emulator.waitNSlots 10


bookingTraceDelay :: EmulatorTrace ()
bookingTraceDelay = do
  h1 <- activateContractWallet (knownWallet 1) endpoints
  let pkh = mockWalletPaymentPubKeyHash $ knownWallet 2
  callEndpoint @"bookTraining" h1 $ BookTrainingParams {
            btTrainer = pkh
          , btBookingId = 10001
          }
  void $ Emulator.waitNSlots 15
  callEndpoint @"delayTraining" h1 $ DelayTrainingParams
    { dTrainer   = pkh
    , dBookingId = 10001
    }
  void $ Emulator.waitNSlots 10


bookingTraceFullfill :: EmulatorTrace ()
bookingTraceFullfill = do
  h1 <- activateContractWallet (knownWallet 1) endpoints
  h2 <- activateContractWallet (knownWallet 2) endpoints
  let cpkh  = mockWalletPaymentPubKeyHash $ knownWallet 1
      tpkh  = mockWalletPaymentPubKeyHash $ knownWallet 2
  callEndpoint @"bookTraining" h1 $ BookTrainingParams {
            btTrainer = tpkh
          , btBookingId = 10001
          }
  void $ Emulator.waitNSlots 41
  callEndpoint @"fullfillTraining" h2 $ FullfillTrainingParams
    { ffClient    = cpkh
    , ffBookingId = 10001
    }
  void $ Emulator.waitNSlots 10

bookingTraceCancel' :: IO ()
bookingTraceCancel' = runEmulatorTraceIO $ bookingTraceCancel

bookingTraceDelay' :: IO ()
bookingTraceDelay' = runEmulatorTraceIO $ bookingTraceDelay

bookingTraceFullfill' :: IO ()
bookingTraceFullfill' = runEmulatorTraceIO $ bookingTraceFullfill
