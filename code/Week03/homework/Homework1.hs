{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeFamilies      #-}

module Homework1 where

import           Plutus.V1.Ledger.Interval (contains)
import           Plutus.V2.Ledger.Api (BuiltinData, POSIXTime, PubKeyHash,
                                       ScriptContext (scriptContextTxInfo), Validator, 
                                       TxInfo (txInfoValidRange),
                                       from, to, mkValidatorScript)
import           Plutus.V2.Ledger.Contexts (txSignedBy)
import           PlutusTx             (compile, unstableMakeIsData)
import           PlutusTx.Prelude     (Bool, traceIfFalse, ($), (&&), (+), (||))
import           Prelude                   (IO, String)
import           Utilities            (Network, posixTimeFromIso8601,
                                            printDataToJSON,
                                            validatorAddressBech32,
                                            wrapValidator, writeValidatorToFile)
import Data.Aeson (Value(Bool))

---------------------------------------------------------------------------------------------------
----------------------------------- ON-CHAIN / VALIDATOR ------------------------------------------

data VestingDatum = VestingDatum
    { beneficiary1 :: PubKeyHash
    , beneficiary2 :: PubKeyHash
    , deadline     :: POSIXTime
    }

unstableMakeIsData ''VestingDatum

{-# INLINABLE mkVestingValidator #-}
-- This should validate if either beneficiary1 has signed the transaction and the current slot is before or at the deadline
-- or if beneficiary2 has signed the transaction and the deadline has passed.
mkVestingValidator :: VestingDatum -> () -> ScriptContext -> Bool
mkVestingValidator _dat () _ctx = traceIfFalse "beneficiary's signature missing" signedByBeneficiary &&
                                  if signedByFirst then traceIfFalse "contract expired for the first beneficiary" deadlineExpired
                                  else traceIfFalse "deadline not reached for the second beneficiary" deadlineReached
  where
    info :: TxInfo
    info = scriptContextTxInfo _ctx

    signedByBeneficiary :: Bool
    signedByBeneficiary = txSignedBy info (beneficiary1 _dat) || txSignedBy info (beneficiary2 _dat)

    signedByFirst :: Bool
    signedByFirst = txSignedBy info $ beneficiary1 _dat

    deadlineReached :: Bool
    deadlineReached = contains (from $ (deadline _dat + 1)) $ txInfoValidRange info

    deadlineExpired :: Bool
    deadlineExpired = contains (to $ deadline _dat) $ txInfoValidRange info

{-# INLINABLE  mkWrappedVestingValidator #-}
mkWrappedVestingValidator :: BuiltinData -> BuiltinData -> BuiltinData -> ()
mkWrappedVestingValidator = wrapValidator mkVestingValidator

validator :: Validator
validator = mkValidatorScript $$(compile [|| mkWrappedVestingValidator ||])