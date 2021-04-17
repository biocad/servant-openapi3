{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}
module Servant.OpenApi.Internal.Orphans where

import Data.OpenApi
import Data.Proxy            (Proxy (..))
import Data.Typeable         (Typeable)
import Servant.Types.SourceT (SourceT)

-- | Pretend that 'SourceT m a' is '[a]'.
--
-- @since 1.1.7
--
instance (Typeable (SourceT m a), ToSchema a) => ToSchema (SourceT m a) where
    declareNamedSchema _ = declareNamedSchema (Proxy :: Proxy [a])
