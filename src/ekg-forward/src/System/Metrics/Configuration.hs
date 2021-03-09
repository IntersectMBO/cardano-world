{- |
Copyright: (c) 2021 Input Output (Hong Kong) Ltd.
Maintainer: Denis Shevchenko <denis.shevchenko@iohk.io>

See README for more info
-}

--
module System.Metrics.Configuration (
    AcceptorConfiguration (..)
  , ForwarderConfiguration (..)
  , HowToConnect (..)
  ) where

import           Data.Text (Text)

type Host = Text
type Port = Int

data HowToConnect
  = LocalPipe    !FilePath
  | RemoteSocket !Host !Port
  deriving (Eq, Show)

data AcceptorConfiguration = AcceptorConfiguration
  { listenToForwarder :: HowToConnect
  } deriving (Eq, Show)

data ForwarderConfiguration = ForwarderConfiguration
  { connectToAcceptor :: HowToConnect
  } deriving (Eq, Show)
