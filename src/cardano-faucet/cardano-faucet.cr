# Cardano Faucet
# - Entry Point
#

require "http/server"
require "http/client"
require "http/params"
require "uri"
require "socket"
require "json"
require "file_utils"
require "db"
require "sqlite3"
require "log"
require "./setup.cr"
require "./general.cr"
require "./cardano-module.cr"

DB.open "sqlite3://last-seen.sqlite" do |db|
  faucet = Cardano::Faucet.new(db)
  middleware = [HTTP::ErrorHandler.new, HTTP::LogHandler.new]
  server = HTTP::Server.new(middleware) do |context|
    context.response.content_type = "application/json"
    status_and_body = faucet.on_request(context)
    context.response.status = status_and_body[:status]
    if context.request.method == "GET" && context.request.path.match(%r(/metrics/?$)) && context.response.status == HTTP::Status::OK
      context.response.print(status_and_body[:body])
    else
      context.response.print(status_and_body[:body].to_json)
    end
  end

  address = server.bind_tcp(FAUCET_LISTEN_ADDRESS, FAUCET_LISTEN_PORT)

  Log.info { "Listening on http://#{address}" }

  Log.info { "ANONYMOUS_ACCESS: #{ANONYMOUS_ACCESS}" }
  Log.info { "ANONYMOUS_ACCESS_ASSETS: #{ANONYMOUS_ACCESS_ASSETS}" }
  Log.info { "ASSETS_TO_GIVE_ANON: #{ASSETS_TO_GIVE_ANON}" }
  Log.info { "CARDANO_ENV: #{CARDANO_ENV}" }
  Log.info { "FAUCET_API_KEY_PATH: #{FAUCET_API_KEY_PATH}" }
  Log.info { "FAUCET_FRONTEND_URL: #{FAUCET_FRONTEND_URL}" }
  Log.info { "FAUCET_LISTEN_ADDRESS: #{FAUCET_LISTEN_ADDRESS}" }
  Log.info { "FAUCET_LISTEN_PORT: #{FAUCET_LISTEN_PORT}" }
  Log.info { "FAUCET_LOG_LEVEL: #{FAUCET_LOG_LEVEL}" }
  Log.info { "FAUCET_PASSPHRASE_PATH: #{FAUCET_PASSPHRASE_PATH}" }
  Log.info { "FAUCET_RECAPTCHA_PATH: #{FAUCET_RECAPTCHA_PATH}" }
  Log.info { "FAUCET_WALLET_ID_PATH: #{FAUCET_WALLET_ID_PATH}" }
  Log.info { "FAUCET_WALLET_ID: #{FAUCET_WALLET_ID}" }
  Log.info { "GENESIS_BLOCK_HASH: #{faucet.settings.genesis_block_hash}" }
  Log.info { "LOVELACES_TO_GIVE_ANON: #{LOVELACES_TO_GIVE_ANON}" }
  Log.info { "LOVELACES_TO_GIVE_APIKEY: #{LOVELACES_TO_GIVE_APIKEY}" }
  Log.info { "RATE_LIMIT_ON_SUCCESS: #{RATE_LIMIT_ON_SUCCESS}" }
  Log.info { "SECS_BETWEEN_REQS_ANON: #{SECS_BETWEEN_REQS_ANON}" }
  Log.info { "SECS_BETWEEN_REQS_APIKEY: #{SECS_BETWEEN_REQS_APIKEY}" }
  Log.info { "SECS_BETWEEN_REQS_ASSETS: #{SECS_BETWEEN_REQS_ASSETS}" }
  Log.info { "USE_BYRON_WALLET: #{USE_BYRON_WALLET}" }
  Log.info { "USE_RECAPTCHA_ON_ANON: #{USE_RECAPTCHA_ON_ANON}" }
  Log.info { "WALLET_API: #{WALLET_API}" }
  Log.info { "WALLET_LISTEN_PORT: #{WALLET_LISTEN_PORT}" }

  server.listen
end
