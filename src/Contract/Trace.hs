{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds        #-}
{-#LANGUAGE ScopedTypeVariables#-}

module Contract.Trace where

import GHC.Natural
import Data.Functor               (void)
import Plutus.Trace
import Wallet.Emulator.Wallet
import Plutus.Trace.Emulator  as Emulator

import Contract.Contract

type NumberOfSlots = Natural

-- Books a training, waits n Slots and performs a cancelation of the booking
bookingTraceCancel :: NumberOfSlots -> EmulatorTrace ()
bookingTraceCancel n = do
  h1 <- activateContractWallet (knownWallet 1) endpoints
  let pkh = mockWalletPaymentPubKeyHash $ knownWallet 2
  callEndpoint @"bookTraining" h1 $ BookTrainingParams {
            btTrainer = pkh
          , btBookingId = 10001
          }
  void $ Emulator.waitNSlots (n :: Natural)
  callEndpoint @"cancelTraining" h1 $ CancelTrainingParams
    { ctTrainer   = pkh
    , ctBookingId = 10001
    }
  void $ Emulator.waitNSlots 10

-- Books a training, waits n Slots and performs a delay of the booking
bookingTraceDelay :: NumberOfSlots -> EmulatorTrace ()
bookingTraceDelay n = do
  h1 <- activateContractWallet (knownWallet 1) endpoints
  let pkh = mockWalletPaymentPubKeyHash $ knownWallet 2
  callEndpoint @"bookTraining" h1 $ BookTrainingParams {
            btTrainer = pkh
          , btBookingId = 10001
          }
  void $ Emulator.waitNSlots n
  callEndpoint @"delayTraining" h1 $ DelayTrainingParams
    { dtTrainer   = pkh
    , dtBookingId = 10001
    }
  void $ Emulator.waitNSlots 10

-- Books a training, waits n Slots and performs a fullfillment of booking by the trainer
bookingTraceFullfill :: NumberOfSlots -> EmulatorTrace ()
bookingTraceFullfill n = do
  h1 <- activateContractWallet (knownWallet 1) endpoints
  h2 <- activateContractWallet (knownWallet 2) endpoints
  let cpkh  = mockWalletPaymentPubKeyHash $ knownWallet 1
      tpkh  = mockWalletPaymentPubKeyHash $ knownWallet 2
  callEndpoint @"bookTraining" h1 $ BookTrainingParams {
            btTrainer = tpkh
          , btBookingId = 10001
          }
  void $ Emulator.waitNSlots n
  callEndpoint @"fullfillTraining" h2 $ FullfillTrainingParams
    { ffClient    = cpkh
    , ffBookingId = 10001
    }
  void $ Emulator.waitNSlots 10

-- Call this from GHC to test the Cancel Endpoint
bookingTraceCancel' :: NumberOfSlots -> IO ()
bookingTraceCancel' input = runEmulatorTraceIO $ bookingTraceCancel $ input

-- Call this from GHC to test the Delay Endpoint
bookingTraceDelay' :: NumberOfSlots -> IO ()
bookingTraceDelay' input = runEmulatorTraceIO $ bookingTraceDelay $ input

-- Call this from GHC to test the FullFill Endpoint
bookingTraceFullfill' :: NumberOfSlots -> IO ()
bookingTraceFullfill' input = runEmulatorTraceIO $ bookingTraceFullfill $ input
