{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module FungibleToken where

import           Control.Monad        hiding (fmap)
import           Data.Aeson           (FromJSON, ToJSON, ToSchema)
-- import           Data.Default         (Default (..))
-- import           Data.Text            (Text)
import           GHC.Generics         (Generic)
import           Plutus.Contract     
-- import qualified PlutusTx             as PlutusTx
-- import           PlutusTx.Prelude     hiding (Semigroup (..), unless)
-- import           Ledger               hiding (singleton)
-- import           Ledger.Ada           as Ada
-- import           Ledger.Constraints   as Constraints
-- import           Ledger.Value         as Value
import           Prelude              (Semigroup (..), Show (..), String)

data Token = Token
    { tokenName        :: "Cloud10"
    , tokenSymbol      :: "LP"
    , tokenTotalSupply :: 1000
    , tokenDecimals    :: 1
    } deriving (Show, Generic, FromJSON, ToJSON, ToSchema)

PlutusTx.makeLift ''Token

{-# INLINABLE mkPolicy #-}
mkPolicy :: Token -> () -> ScriptContext -> Bool
mkPolicy token () ctx =
    let
        info = scriptContextTxInfo ctx
        value = txInfoValue info
        ownOutput = txOutValue ownOutputInfo
        ownOutputInfo = case findOwnOutput ctx of
            Nothing -> traceError "contract output not found"
            Just i  -> txOutInfo $ txInfoOutputs info !! i
        isValidSupply = tokenTotalSupply token == assetClassValueOf value (AssetClass (tokenSymbol token, 1))
        isValidDecimals = tokenDecimals token >= tokenDecimalsOf value (AssetClass (tokenSymbol token, 1))
    in
        isValidSupply && isValidDecimals

policy :: Token -> Scripts.MintingPolicy
policy token = mkMintingPolicyScript $
    $$(PlutusTx.compile [|| Scripts.wrapMintingPolicy . mkPolicy ||])
    `PlutusTx.applyCode`
    PlutusTx.liftCode token

type TokenSchema =
    BlockchainActions
        .\/ Endpoint "mint" Token
        .\/ Endpoint "transfer" (CurrencySymbol, TokenName, Integer, PubKeyHash)

mint :: Token -> Contract w s Text ()
mint token = do
    pk <- pubKeyHash <$> ownPubKey
    let cs = fromString $ tokenSymbol token
        tn = fromString $ tokenName token
        v = assetClassValue (AssetClass (cs, tn)) 1
        tx = mustMintValue v
    ledgerTx <- submitTxConstraints (policy token) tx
    void $ awaitTxConfirmed $ txId ledgerTx
    logInfo @String $ "Minted " ++ show v ++ " of " ++ show cs ++ "." ++ show tn ++ " for " ++ show pk

transfer :: (CurrencySymbol, TokenName, Integer, PubKeyHash) -> Contract w s Text ()
transfer (cs, tn, amt, to) = do
    pk <- pubKeyHash <$> ownPubKey
    let v = Value.singleton cs tn amt
        tx = Constraints.mustPayToPubKey to v
    ledgerTx <- submitTxConstraintsSpending (singleton pk v) tx
   
