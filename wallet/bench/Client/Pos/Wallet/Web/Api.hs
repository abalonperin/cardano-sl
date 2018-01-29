{-# LANGUAGE TypeOperators #-}

-- | Client functions for Wallet Web API. To be more precise,
-- only for endpoints we have to benchmark.

module Client.Pos.Wallet.Web.Api
    ( getHistory
    , getWallet
    , getWallets
    , newPayment
    ) where

import           Universum

import           Servant.API                ((:<|>) (..), (:>))
import           Servant.Client             (ClientM, client)

import           Pos.Client.Txp.Util        (InputSelectionPolicy)
import           Pos.Core.Types             (Coin)
import           Pos.Wallet.Web.Api         (ApiPrefix, GetHistory, GetWallet,
                                             GetWallets, NewPayment)
import           Pos.Wallet.Web.ClientTypes (Addr, CAccountId (..),
                                             CId (..), CPassPhrase, CTx, CWallet,
                                             ScrollLimit, ScrollOffset, Wal)

-- | "Benchmarking API" which includes
-- endpoints we need for benchmarking.
type WalletBenchApi = ApiPrefix :> (
     GetHistory
    :<|>
     GetWallet
    :<|>
     GetWallets
    :<|>
     NewPayment
    )

-- | Clients for "Benchmarking API".
getHistory
    :: Maybe (CId Wal)
    -> Maybe CAccountId
    -> Maybe (CId Addr)
    -> Maybe ScrollOffset
    -> Maybe ScrollLimit
    -> ClientM ([CTx], Word)
getWallet
    :: CId Wal
    -> ClientM CWallet
getWallets
    :: ClientM [CWallet]
newPayment
    :: Maybe CPassPhrase
    -> CAccountId
    -> CId Addr
    -> Coin
    -> Maybe InputSelectionPolicy
    -> ClientM CTx
getHistory :<|> getWallet :<|> getWallets :<|> newPayment =
    client (Proxy @WalletBenchApi)