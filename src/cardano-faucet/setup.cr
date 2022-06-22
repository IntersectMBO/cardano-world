# Cardano Faucet
# - Setup and config
#

ANONYMOUS_ACCESS         = ENV.fetch("ANONYMOUS_ACCESS", "TRUE") == "TRUE" ? true : false
ANONYMOUS_ACCESS_ASSETS  = ENV.fetch("ANONYMOUS_ACCESS_ASSETS", "TRUE") == "TRUE" ? true : false
ASSETS_TO_GIVE_ANON      = ENV.fetch("ASSETS_TO_GIVE_ANON", "2").to_u64
CARDANO_ENV              = ENV.fetch("CARDANO_ENV", "DEFAULT")
FAUCET_API_KEY_PATH      = ENV.fetch("FAUCET_API_KEY_PATH", "/var/lib/cardano-faucet/faucet.apikey")
FAUCET_FRONTEND_URL      = ENV.fetch("FAUCET_FRONTEND_URL", "")
FAUCET_LOG_LEVEL         = ENV.fetch("CRYSTAL_LOG_LEVEL", "INFO")
FAUCET_LOG_SOURCES       = ENV.fetch("CRYSTAL_LOG_SOURCES", "*")
FAUCET_LISTEN_ADDRESS    = ENV.fetch("FAUCET_LISTEN_ADDRESS", "127.0.0.1")
FAUCET_LISTEN_PORT       = ENV.fetch("FAUCET_LISTEN_PORT", "8091").to_i
FAUCET_PASSPHRASE_PATH   = ENV.fetch("FAUCET_SECRET_PASSPHRASE_PATH", "/var/lib/cardano-faucet/faucet.passphrase")
FAUCET_RECAPTCHA_PATH    = ENV.fetch("FAUCET_SECRET_RECAPTCHA_PATH", "/var/lib/cardano-faucet/faucet.recaptcha")
FAUCET_WALLET_ID_PATH    = ENV.fetch("FAUCET_WALLET_ID_PATH", "/var/lib/cardano-faucet/faucet.id")
LOVELACES_TO_GIVE_ANON   = ENV.fetch("LOVELACES_TO_GIVE_ANON", "1000000000").to_u64
LOVELACES_TO_GIVE_APIKEY = ENV.fetch("LOVELACES_TO_GIVE_APIKEY", "1000000000").to_u64
RATE_LIMIT_ON_SUCCESS    = ENV.fetch("RATE_LIMIT_ON_SUCCESS", "TRUE") == "TRUE" ? true : false
SECS_BETWEEN_REQS_ANON   = ENV.fetch("SECS_BETWEEN_REQS_ANON", "86400").to_u32
SECS_BETWEEN_REQS_APIKEY = ENV.fetch("SECS_BETWEEN_REQS_APIKEY", "0").to_u32
SECS_BETWEEN_REQS_ASSETS = ENV.fetch("SECS_BETWEEN_REQS_ASSETS", "86400").to_u32
USE_BYRON_WALLET         = ENV.fetch("USE_BYRON_WALLET", "FALSE") == "TRUE" ? true : false
USE_RECAPTCHA_ON_ANON    = ENV.fetch("USE_RECAPTCHA_ON_ANON", "TRUE") == "TRUE" ? true : false
WALLET_LISTEN_PORT       = ENV.fetch("WALLET_LISTEN_PORT", "8090").to_i
WALLET_API               = ENV.fetch("WALLET_API", "http://localhost:#{WALLET_LISTEN_PORT}/v2")

FAUCET_WALLET_ID  = readFile(FAUCET_WALLET_ID_PATH)
RECAPTCHA_SECRET  = readFile(FAUCET_RECAPTCHA_PATH)
SECRET_PASSPHRASE = readFile(FAUCET_PASSPHRASE_PATH)
API_KEYS          = readKeys(FAUCET_API_KEY_PATH)

API_KEY_LEN                 = 32_u8
API_KEY_COMMENT_MAX_LEN     = 64_u8
API_KEY_UNIT_POLICY_ID_LEN  = 56_u8
API_KEY_UNIT_ASSET_NAME_LEN = 64_u8
API_KEY_UNIT_TYPE_DELIMITER = ""

API_URI = URI.parse("#{WALLET_API}")
HEADERS = HTTP::Headers{"Content-Type" => "application/json; charset=utf-8"}

RECAPTCHA_URI    = "https://www.google.com/recaptcha/api/siteverify"
RECAPTCHA_HEADER = HTTP::Headers{"Content-Type" => "application/x-www-form-urlencoded; charset=utf-8"}

MIN_METRICS_PERIOD = 10_u8

STDOUT.sync = true
Log.setup_from_env

IO_CMD_OUT = IO::Memory.new
IO_CMD_ERR = IO::Memory.new
