-- | The kernel of the wallet implementation
--
-- The goal is to keep this module mostly self-contained, and not use to many
-- Cardano specific types (except those types that appear in the translation
-- of the UTxO DSL).
module Cardano.Wallet.Kernel (
    -- * Passive wallet
    PassiveWallet -- opaque
  , bracketPassiveWallet
  , init
    -- * Active wallet
  , ActiveWallet -- opaque
  , bracketActiveWallet
  ) where

import Universum

import Cardano.Wallet.Kernel.Diffusion (WalletDiffusion(..))

import Pos.Core (TxAux)

{-------------------------------------------------------------------------------
  Passive wallet
-------------------------------------------------------------------------------}

-- | Passive wallet
--
-- A passive wallet can receive and process blocks, keeping track of state,
-- but cannot send new transactions.
--
-- TODO: This is just a placeholder for now, we'll want all kinds of state
-- in here.
data PassiveWallet = PassiveWallet

-- | Allocate wallet resources
--
-- NOTE: See also 'init'.
--
-- TODO: Here and elsewhere we'll want some constraints on this monad here, but
-- it shouldn't be too specific.
bracketPassiveWallet :: MonadMask m
                     => (PassiveWallet -> m a) -> m a
bracketPassiveWallet =
    bracket
      (return PassiveWallet)
      (\_ -> return ())

-- | Initialize the wallet
--
-- This is separate from allocating the wallet resources, and will only be
-- called when the node is initialized (when run in the node proper).
init :: PassiveWallet -> IO ()
init _w = return ()

{-------------------------------------------------------------------------------
  Active wallet
-------------------------------------------------------------------------------}

-- | Active wallet
--
-- An active wallet can do everything the passive wallet can, but also
-- send new transactions.
data ActiveWallet = ActiveWallet {
      -- | The underlying passive wallet
      walletPassive :: PassiveWallet

      -- | The wallet diffusion layer
    , walletDiffusion :: WalletDiffusion
    }

bracketActiveWallet :: MonadMask m
                    => PassiveWallet
                    -> WalletDiffusion
                    -> (ActiveWallet -> m a) -> m a
bracketActiveWallet walletPassive walletDiffusion =
    bracket
      (return ActiveWallet{..})
      (\_ -> return ())
