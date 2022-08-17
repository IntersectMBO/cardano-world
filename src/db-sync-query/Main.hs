-- #!/usr/bin/env stack
{-# LANGUAGE RecordWildCards #-}

import Control.Monad
import qualified Data.Text as T
import Network.HTTP
import Network.Socket
-- import System.IO
import Text.URI
import qualified Network.URI as NU
import qualified Database.PostgreSQL.Simple as PG
import System.Environment
import Data.Maybe
import qualified GHC.Word as W
import Text.Printf

data QueryParams = QueryParams
                 { epoch :: String
                 , majorVersion :: String
                 , minorVersion :: String
                 }
                 deriving (Show)

uriToQueryParams :: String -> Either String QueryParams
uriToQueryParams uriString = if uri == Nothing then Left "not able to parse"
                             else if epoch == Nothing then Left "epoch needs to be set and an integer"
                             else if majorVersion == Nothing then Left "majorVersion needs to be set and an integer"
                             else if minorVersion == Nothing then Left "minorVersion needs to be set and an integer"
                             else Right $ QueryParams (fromJust epoch) (fromJust majorVersion) (fromJust minorVersion)
  where
    uri = mkURI (stringToText uriString)
    query = uriQuery $ fromJust uri
    getKey term = if value == [] then Nothing else Just ((\(QueryParam _ v) -> textToString (unRText v)) (head value))
      where
        checkForID (QueryParam name _) = term == textToString (unRText name)
        checkForID (QueryFlag _) = False
        value = filter checkForID query

    epoch = getKey "epoch"
    majorVersion = getKey "majorVersion"
    minorVersion = getKey "minorVersion"

textToString :: T.Text -> String
textToString = T.unpack

stringToText :: String -> T.Text
stringToText = T.pack

newtype ServerConfigs = ServerConfigs
  { _port :: Port Int
  }

defaultServerConfigs :: ServerConfigs
defaultServerConfigs = ServerConfigs {_port = Just 8080}

type Port = Maybe

-- | A flip fromMaybe
(//) :: Maybe a -> a -> a
Just x // _ = x
Nothing // y = y
-- ^ Try something new

startServer :: ServerConfigs -> IO ()
startServer ServerConfigs {..} = do
  sock <- socket AF_INET Stream defaultProtocol
  let hints = defaultHints {addrFlags = [AI_NUMERICHOST], addrSocketType = Stream}
  let ourDefaultPort = 8080
  let customPort = return $ show $ _port // ourDefaultPort
  addr : _ <- getAddrInfo (Just hints) (Just "0.0.0.0") customPort
  let address = addrAddress addr
  pgHost <- getEnv "DB_HOST"
  pgPort <- getEnv "DB_PORT"
  pgDatabase <- getEnv "DB_NAME"
  pgUser <- getEnv "DB_USER"
  pgPass <- getEnv "DB_PASS"
  let localPG = PG.defaultConnectInfo
        { PG.connectHost = pgHost
        , PG.connectPort = (read pgPort :: W.Word16)
        , PG.connectDatabase = pgDatabase
        , PG.connectUser = pgUser
        , PG.connectPassword = pgPass
        }
  pgConn <- PG.connect localPG
  putStrLn $ "Server is listening at http://" <> show address
  bind sock $ addrAddress addr
  listen sock 1
  forever $ do
    (csock, _) <- accept sock
    hs <- socketConnection "" (_port // ourDefaultPort) csock
    req <- receiveHTTP hs
    case req of
      Left _ -> do
        respondHTTP hs $
          Response (4, 0, 4) "Not found" [] "ERROR 404"
        Network.HTTP.close hs
      Right (Request rqURI _ _ _) -> do
        case (uriToQueryParams (NU.uriToString id rqURI "")) of
          Left errorText -> do
            respondHTTP hs $
              Response (4, 0, 4) "Not found" [] ("ERROR malformed query parameters: " ++ errorText)
            Network.HTTP.close hs
          Right params -> do
            putStrLn $ "responding to request with params:" <> show params
            ((PG.Only queryBlocks):_) <- PG.query pgConn "SELECT COUNT(*) FROM block WHERE epoch_no = ? AND proto_major = ? AND proto_minor = ?"
              $ (epoch params, majorVersion params, minorVersion params) :: IO [PG.Only Int]
            ((PG.Only totalBlocks):_) <- PG.query pgConn "SELECT COUNT(*) FROM block WHERE epoch_no = ?"
              $ (PG.Only (epoch params)) :: IO [PG.Only Int]
            let output = printf "found blocks: %d; total blocks: %d;" queryBlocks totalBlocks
            putStrLn $ printf "query results => " ++ output
            respondHTTP hs $
              Response (2, 0, 0) "Not found" [] output
            Network.HTTP.close hs

main :: IO ()
main = startServer defaultServerConfigs
