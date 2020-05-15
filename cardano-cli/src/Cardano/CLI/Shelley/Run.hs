
module Cardano.CLI.Shelley.Run
  ( ShelleyClientCmdError
  , runShelleyClientCommand
  ) where

import           Cardano.Prelude

import           Control.Monad.Trans.Except (ExceptT)
import           Control.Monad.Trans.Except.Extra (firstExceptT)

import           Cardano.CLI.Shelley.Parsers

import           Cardano.CLI.Shelley.Run.Address
import           Cardano.CLI.Shelley.Run.StakeAddress
import           Cardano.CLI.Shelley.Run.Transaction
import           Cardano.CLI.Shelley.Run.Node
import           Cardano.CLI.Shelley.Run.Pool
import           Cardano.CLI.Shelley.Run.Query
                                         -- Block, System, DevOps
import           Cardano.CLI.Shelley.Run.Genesis
import           Cardano.CLI.Shelley.Run.TextView


data ShelleyClientCmdError
  = ShelleyCmdAddressError !ShelleyAddressCmdError
  | ShelleyCmdGenesisError !ShelleyGenesisCmdError
  | ShelleyCmdNodeError !ShelleyNodeCmdError
  | ShelleyCmdPoolError !ShelleyPoolCmdError
  | ShelleyCmdStakeAddressError !ShelleyStakeAddressCmdError
  | ShelleyCmdTextViewError !ShelleyTextViewFileError
  | ShelleyCmdTransactionError !ShelleyTxCmdError
  | ShelleyCmdQueryError !ShelleyQueryCmdError
  deriving Show


--
-- CLI shelley command dispatch
--

runShelleyClientCommand :: ShelleyCommand -> ExceptT ShelleyClientCmdError IO ()
runShelleyClientCommand (AddressCmd      cmd) = firstExceptT ShelleyCmdAddressError $ runAddressCmd cmd
runShelleyClientCommand (StakeAddressCmd cmd) = firstExceptT ShelleyCmdStakeAddressError $ runStakeAddressCmd cmd
runShelleyClientCommand (TransactionCmd  cmd) = firstExceptT ShelleyCmdTransactionError $ runTransactionCmd  cmd
runShelleyClientCommand (NodeCmd         cmd) = firstExceptT ShelleyCmdNodeError $ runNodeCmd cmd
runShelleyClientCommand (PoolCmd         cmd) = firstExceptT ShelleyCmdPoolError $ runPoolCmd cmd
runShelleyClientCommand (QueryCmd        cmd) = firstExceptT ShelleyCmdQueryError $ runQueryCmd cmd
runShelleyClientCommand (BlockCmd        cmd) = runBlockCmd cmd
runShelleyClientCommand (SystemCmd       cmd) = runSystemCmd cmd
runShelleyClientCommand (DevOpsCmd       cmd) = runDevOpsCmd cmd
runShelleyClientCommand (GenesisCmd      cmd) = firstExceptT ShelleyCmdGenesisError $ runGenesisCmd cmd
runShelleyClientCommand (TextViewCmd     cmd) = firstExceptT ShelleyCmdTextViewError $ runTextViewCmd cmd



--TODO: if you fill any of these in, move them into their own modules first!

runBlockCmd :: BlockCmd -> ExceptT ShelleyClientCmdError IO ()
runBlockCmd cmd = liftIO $ putStrLn $ "TODO: runBlockCmd: " ++ show cmd

runSystemCmd:: SystemCmd -> ExceptT ShelleyClientCmdError IO ()
runSystemCmd cmd = liftIO $ putStrLn $ "TODO: runSystemCmd: " ++ show cmd

runDevOpsCmd :: DevOpsCmd -> ExceptT ShelleyClientCmdError IO ()
runDevOpsCmd cmd = liftIO $ putStrLn $ "TODO: runDevOpsCmd: " ++ show cmd
