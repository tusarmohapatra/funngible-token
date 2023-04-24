{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}



import            Plutus.Contrct   ()
import            PlutusTx         ()
import            PlutusTx.Prelude ()
import            Prelude          ()


data Token = Token { tokenName :: "Cloud10"
                   , tokenSymbole :: "LP"
                   , tokenDecimals :: 1
                   , tokenTotalSupply :: 1000
                   } deriving(Generic, ToJSON, FromJSON, ToSchema)

PlutusTx.makeLift ''Token


mkValidator :: Token -> () -> Contract FungibleToken text ()
mkValidator token () = do

tokenContract :: Token -> FungibleToken text () 
tokenContract 


