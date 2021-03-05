{- |
Copyright: (c) 2021 Input Output (Hong Kong) Ltd.
Maintainer: Denis Shevchenko <denis.shevchenko@iohk.io>

See README for more info
-}

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE PolyKinds #-}

--
module System.Metrics.Type (
    Req (..)
  , Resp (..)
  ) where

import           GHC.Generics (Generic)

import qualified Codec.Serialise as CBOR

import           Ouroboros.Network.Util.ShowProxy (ShowProxy(..))

data Req = SimpleReq
  deriving (Generic, Show)

data Resp = SimpleResp ![Int]
  deriving (Generic, Show)

instance ShowProxy Req where
  showProxy _ = "SimpleReq"

instance ShowProxy Resp where
  showProxy _ = "SimpleResp" 

instance CBOR.Serialise Req
instance CBOR.Serialise Resp
