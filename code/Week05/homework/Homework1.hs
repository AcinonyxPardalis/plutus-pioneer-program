{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE OverloadedStrings #-}

module Homework1 where

import           Plutus.V2.Ledger.Api (BuiltinData, MintingPolicy, POSIXTime,
                                       PubKeyHash, ScriptContext (scriptContextTxInfo),
                                       mkMintingPolicyScript)
import qualified PlutusTx
import           PlutusTx.Prelude     (Bool (False), ($), trace, traceIfFalse, (&&))
import           Utilities            (wrapPolicy)
-- import Plutus.V1.Ledger.Contexts (txSignedBy, ScriptContext (scriptContextPurpose), TxInfo (txInfoValidRange))
import Plutus.V1.Ledger.Interval
import Plutus.V2.Ledger.Contexts

{-# INLINABLE mkDeadlinePolicy #-}
-- This policy should only allow minting (or burning) of tokens if the owner of the specified PubKeyHash
-- has signed the transaction and if the specified deadline has not passed.
mkDeadlinePolicy :: PubKeyHash -> POSIXTime -> () -> ScriptContext -> Bool
mkDeadlinePolicy _pkh _deadline () _ctx = traceIfFalse "missing signature" signed 
                                        && traceIfFalse "missed deadline" validTime
    where
        tx = scriptContextTxInfo _ctx
        signed = txSignedBy tx _pkh
        validTime = contains (to _deadline) $ txInfoValidRange tx
        

{-# INLINABLE mkWrappedDeadlinePolicy #-}
mkWrappedDeadlinePolicy :: PubKeyHash -> POSIXTime -> BuiltinData -> BuiltinData -> ()
mkWrappedDeadlinePolicy pkh deadline = wrapPolicy $ mkDeadlinePolicy pkh deadline

deadlinePolicy :: PubKeyHash -> POSIXTime -> MintingPolicy
deadlinePolicy pkh deadline = mkMintingPolicyScript $
    $$(PlutusTx.compile [|| mkWrappedDeadlinePolicy ||])
        `PlutusTx.applyCode` PlutusTx.liftCode pkh
        `PlutusTx.applyCode` PlutusTx.liftCode deadline
