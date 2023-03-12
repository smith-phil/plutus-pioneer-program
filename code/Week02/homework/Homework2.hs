{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE TemplateHaskell     #-}

module Homework2 where

import qualified Plutus.V2.Ledger.Api as PlutusV2
import           PlutusTx             (unstableMakeIsData, compile)
import           PlutusTx.Prelude     (Bool, BuiltinData, (/=))
import           Utilities            (wrap, writeValidatorToFile)
import           Prelude              (IO)
---------------------------------------------------------------------------------------------------
----------------------------------- ON-CHAIN / VALIDATOR ------------------------------------------

data MyRedeemer = MyRedeemer
    { flag1 :: Bool
    , flag2 :: Bool
    }

PlutusTx.unstableMakeIsData ''MyRedeemer

-- Create a validator that unlocks the funds if MyRedemeer's flags are different
mkValidator :: () -> MyRedeemer -> PlutusV2.ScriptContext -> Bool
-- Takes validator and Bool flags as second argument, returns if they are not equal
mkValidator _ (MyRedeemer f s) _ = f /= s
{-# INLINABLE mkValidator #-}

wrappedVal :: BuiltinData -> BuiltinData -> BuiltinData -> ()
wrappedVal = Utilities.wrap mkValidator
{-# INLINABLE wrappedVal #-}

validator :: PlutusV2.Validator
validator = PlutusV2.mkValidatorScript $$(PlutusTx.compile [|| wrappedVal ||])

---------------------------------------------------------------------------------------------------
------------------------------------- HELPER FUNCTIONS --------------------------------------------
saveVal :: IO()
saveVal = Utilities.writeValidatorToFile "./assets/homeworktwo.plutus" validator
