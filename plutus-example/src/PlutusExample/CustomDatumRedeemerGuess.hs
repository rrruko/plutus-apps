{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeFamilies      #-}

module PlutusExample.CustomDatumRedeemerGuess
  where

import PlutusTx.Prelude
import qualified PlutusTx
import Ledger
import qualified Ledger.Typed.Scripts as Scripts

data PoolDatum
  = PoolDatum { ident :: !Ident }

-- `Ident`, named for its most frequent use as an identifier, is basically an
-- `Integer` encoded as a `ByteString`, because Plutus offers no way to do this
-- in its standard library.
newtype Ident = Ident BuiltinByteString
  deriving newtype (PlutusTx.ToData, PlutusTx.FromData, PlutusTx.UnsafeFromData)

instance Eq Ident where
  {-# inlinable (==) #-}
  Ident b == Ident b' = b == b'

newtype PoolRedeemer = PoolRedeemer Integer

PlutusTx.makeIsDataIndexed ''PoolDatum [('PoolDatum, 0)]
PlutusTx.makeIsDataIndexed ''PoolRedeemer [('PoolRedeemer, 0)]

{-# inlinable poolContract #-}
poolContract
  :: PoolDatum
  -> PoolRedeemer
  -> ScriptContext
  -> Bool
poolContract (PoolDatum _) (PoolRedeemer _) _ctx = True

poolScript :: Validator
poolScript =
  mkValidatorScript
    ($$(PlutusTx.compile [|| wrap ||]))
  where wrap = Scripts.mkUntypedValidator poolContract
