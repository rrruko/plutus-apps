{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
module Spec.ScriptContext.V1.RequiredSigner(tests) where

import Control.Monad (void)
import Data.Map qualified as Map
import Data.Void (Void)
import Test.Tasty (TestTree, testGroup)

import Data.String (fromString)
import Ledger (PaymentPubKey (PaymentPubKey), PaymentPubKeyHash (unPaymentPubKeyHash))
import Ledger qualified
import Ledger.Ada qualified as Ada
import Ledger.Address (PaymentPubKeyHash (PaymentPubKeyHash))
import Ledger.Constraints.OffChain qualified as Constraints (otherScript, paymentPubKey, typedValidatorLookups,
                                                             unspentOutputs)
import Ledger.Constraints.TxConstraints (requiredSignatories)
import Ledger.Constraints.TxConstraints qualified as Constraints (mustBeSignedBy, mustIncludeDatum, mustPayToTheScript,
                                                                  mustSpendScriptOutput)
import Ledger.Tx qualified as Tx
import Ledger.Typed.Scripts qualified as Scripts
import Plutus.Contract as Con
import Plutus.Contract.Test (assertFailedTransaction, assertValidatedTransactionCount, checkPredicateOptions,
                             defaultCheckOptions, mockWalletPaymentPubKey, mockWalletPaymentPubKeyHash, w1, w2)
import Plutus.Trace qualified as Trace
import Plutus.V1.Ledger.Api
import Plutus.V1.Ledger.Contexts (txSignedBy)
import Plutus.V1.Ledger.Scripts (ScriptError (EvaluationError), unitDatum)
import PlutusTx qualified
import PlutusTx.Prelude (traceIfFalse)
import Prelude hiding (not)

tests :: TestTree
tests =
    testGroup "required signer"
        [ ownWallet
        , missingRequiredSignature
        , otherWallet
        --, defaultWallet -- no need to explictly use mustBeSignedBy
        ]

contract :: PaymentPubKey -> PaymentPubKeyHash -> Contract () Empty ContractError ()
contract pk pkh = do
    let lookups1 = Constraints.typedValidatorLookups typedValidator
        tx1 = Constraints.mustPayToTheScript () (Ada.lovelaceValueOf 25000000)
    ledgerTx1 <- submitTxConstraintsWith lookups1 tx1
    awaitTxConfirmed $ Tx.getCardanoTxId ledgerTx1

    utxos <- utxosAt scrAddress
    let orefs = fst <$> Map.toList utxos
        redeemer = Redeemer $ toBuiltinData $ unPaymentPubKeyHash pkh
        lookups2 =
            Constraints.otherScript validatorScript
            <> Constraints.unspentOutputs utxos
            <> Constraints.paymentPubKey pk
        tx2 =
            foldMap (\oref -> Constraints.mustSpendScriptOutput oref redeemer) orefs
            <> Constraints.mustIncludeDatum unitDatum
            <> Constraints.mustBeSignedBy pkh
    logInfo @String $ "Required Signatories: " ++ show (requiredSignatories tx2)
    ledgerTx2 <- submitTxConstraintsWith @Void lookups2 tx2
    awaitTxConfirmed $ Tx.getCardanoTxId ledgerTx2

ownWallet :: TestTree
ownWallet =
    let pk  = mockWalletPaymentPubKey     w1
        pkh = mockWalletPaymentPubKeyHash w1
        trace = do
            void $ Trace.activateContractWallet w1 $ contract pk pkh
            void $ Trace.waitNSlots 3
    in checkPredicateOptions defaultCheckOptions "validation check for required signer using own wallet" (assertValidatedTransactionCount 2) (void trace)

missingRequiredSignature :: TestTree -- shouldn't this fail in the contract before script validation?
missingRequiredSignature =
    let pk  = mockWalletPaymentPubKey     w1
        pkh = PaymentPubKeyHash $ fromString "76aaef06f38cc98ed08ceb168ddb55bab2ea5df43a6847a99f086fc1" :: PaymentPubKeyHash
        trace = do
            void $ Trace.activateContractWallet w1 $ contract pk pkh
            void $ Trace.waitNSlots 3
    in checkPredicateOptions defaultCheckOptions "validation throws error when required signature is missing"
    (assertFailedTransaction (\_ err _ -> case err of {Ledger.ScriptFailure (EvaluationError ("Missing signature":_) _) -> True; _ -> False  }))
    (void trace)

otherWallet :: TestTree -- FAILS: Correct required signer in txbody but no signatures listed in balanced tx
otherWallet =
    let pk  = mockWalletPaymentPubKey     w2
        pkh = mockWalletPaymentPubKeyHash w2
        trace = do
            void $ Trace.activateContractWallet w1 $ contract pk pkh
            void $ Trace.waitNSlots 3
    in checkPredicateOptions defaultCheckOptions "transaction is failing when checking other wallet has signed tx" (assertValidatedTransactionCount 2) (void trace)

{-# INLINEABLE mkValidator #-}
mkValidator :: () -> PubKeyHash -> ScriptContext -> Bool
mkValidator _ pkh ctx = traceIfFalse "Missing signature" (txSignedBy info pkh)
    where
    info :: TxInfo
    info = scriptContextTxInfo ctx

data UnitTest
instance Scripts.ValidatorTypes UnitTest  where
    type instance DatumType UnitTest = ()
    type instance RedeemerType UnitTest = PubKeyHash

typedValidator :: Scripts.TypedValidator UnitTest
typedValidator = Scripts.mkTypedValidator @UnitTest
    $$(PlutusTx.compile [||mkValidator||])
    $$(PlutusTx.compile [|| wrap ||])
    where
        wrap = Scripts.mkUntypedValidator

validatorScript :: Validator
validatorScript = Scripts.validatorScript typedValidator

valHash :: ValidatorHash
valHash = Scripts.validatorHash typedValidator

scrAddress :: Ledger.Address
scrAddress = Ledger.scriptHashAddress valHash
