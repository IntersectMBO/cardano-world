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


module System.Metrics.Typed.Protocol where

import           System.Metrics.Typed.Core

data ReqResp req resp where
  StIdle :: ReqResp req resp
  StBusy :: ReqResp req resp
  StDone :: ReqResp req resp

instance Protocol (ReqResp req resp) where

  data Message (ReqResp req resp) from to where
    MsgReq  :: req  -> Message (ReqResp req resp) StIdle StBusy
    MsgResp :: resp -> Message (ReqResp req resp) StBusy StIdle
    MsgDone ::         Message (ReqResp req resp) StIdle StDone

  data AcceptorHasAgency st where
    TokIdle :: AcceptorHasAgency StIdle

  data ForwarderHasAgency st where
    TokBusy :: ForwarderHasAgency StBusy

  data NobodyHasAgency st where
    TokDone :: NobodyHasAgency StDone

  exclusionLemma_AcceptorAndForwarderHaveAgency TokIdle tok = case tok of {}
  exclusionLemma_NobodyAndAcceptorHaveAgency TokDone tok = case tok of {}
  exclusionLemma_NobodyAndForwarderHaveAgency TokDone tok = case tok of {}


deriving instance (Show req, Show resp)
               => Show (Message (ReqResp req resp) from to)

deriving instance (Eq req, Eq resp)
               => Eq (Message (ReqResp req resp) from to)

instance Show (AcceptorHasAgency (st :: ReqResp req resp)) where
    show TokIdle = "TokIdle"

instance Show (ForwarderHasAgency (st :: ReqResp req resp)) where
    show TokBusy = "TokBusy"
