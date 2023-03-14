{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE InstanceSigs #-}

module Homework1 where

import           Plutus.V2.Ledger.Api (BuiltinData, POSIXTime, PubKeyHash,
                                       ScriptContext (scriptContextTxInfo),
                                       Validator, mkValidatorScript, TxInfo (txInfoValidRange),
                                       to, from)
import           PlutusTx             (compile, unstableMakeIsData)
import           PlutusTx.Prelude     (Bool (..), traceIfFalse, ($), (&&))
import           Utilities            (wrap)
import Plutus.V2.Ledger.Contexts (txSignedBy)
import Plutus.V1.Ledger.Interval (contains, after, overlaps, before)

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
mkVestingValidator _dat () _ctx = 
    if firstBeneficiaryDeadlineValid 
        then signedByFirstBeneficiary    
        else 
            secondBeneficiaryDeadlineValid && signedBySecondBeneficiary
    where
        info :: TxInfo
        info = scriptContextTxInfo _ctx

        signedByFirstBeneficiary :: Bool
        signedByFirstBeneficiary = txSignedBy info $ beneficiary1 _dat 

        signedBySecondBeneficiary :: Bool
        signedBySecondBeneficiary = txSignedBy info $ beneficiary2 _dat 

        firstBeneficiaryDeadlineValid :: Bool 
        firstBeneficiaryDeadlineValid = contains (to $ deadline _dat) $ txInfoValidRange info

        secondBeneficiaryDeadlineValid :: Bool 
        secondBeneficiaryDeadlineValid = before (deadline _dat) (txInfoValidRange info)



{-# INLINABLE  mkWrappedVestingValidator #-}
mkWrappedVestingValidator :: BuiltinData -> BuiltinData -> BuiltinData -> ()
mkWrappedVestingValidator = wrap mkVestingValidator

validator :: Validator
validator = mkValidatorScript $$(compile [|| mkWrappedVestingValidator ||])
