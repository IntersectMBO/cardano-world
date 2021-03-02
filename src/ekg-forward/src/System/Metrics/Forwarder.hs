{- |
Copyright: (c) 2021 Input Output (Hong Kong) Ltd.
Maintainer: Denis Shevchenko <denis.shevchenko@iohk.io>

See README for more info
-}

{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE PolyKinds #-}


module System.Metrics.Forwarder where

-- This top-level module will be used by the forwarder app
-- (the app that collects EKG metrics and sends them to the acceptor). 
