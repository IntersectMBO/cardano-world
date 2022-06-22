require "./classes/account.cr"
require "./classes/api-exception.cr"
require "./classes/fees.cr"
require "./classes/settings.cr"
require "./classes/txs.cr"
require "./classes/wallet.cr"

module Cardano
  class Faucet
    getter settings

    alias Allow = Bool
    alias Response = {status: HTTP::Status, body: SendFundsResult | NotFoundResult | RateLimitResult | String}
    alias SendFundsResult = {success: Bool, amount: UInt64, unit: String, fee: Int64, minLovelace: Int64, txid: String}
    alias RateLimitResult = {statusCode: Int32, error: String, message: String, retryAfter: Time}
    alias NotFoundResult = {statusCode: Int32, error: String, message: String}

    @settings : Settings
    @db : DB::Database
    @lastMetricsTime : Time
    @lastMetrics : String

    def initialize(@db)
      @settings = Settings.get
      @db.scalar "PRAGMA journal_mode=WAL"
      @lastMetricsTime = Time.utc
      @lastMetrics = ""
      @lastRequestTime = Time.utc
      migrations
    end

    # Recursively increment version until no migrations are left
    # Note that we cannot `DROP COLUMN`
    # see https://www.sqlite.org/faq.html#q11
    def migrations
      version = @db.scalar("PRAGMA main.user_version").as(Int64)

      case version
      when 0
        Log.info { "Performing db migration 0: request table creation" }
        migrate <<-SQL
          CREATE TABLE IF NOT EXISTS requests (
            host VARCHAR UNIQUE PRIMARY KEY NOT NULL,
            seen TIME NOT NULL DEFAULT CURRENT_TIMESTAMP
          )
        SQL
      when 1
        Log.info { "Performing db migration 1: adding a genesis hash column" }
        migrate <<-SQL
          ALTER TABLE requests ADD COLUMN hash VARCHAR NOT NULL DEFAULT ''
        SQL
      when 2
        # Add support for per API key rate limiting
        Log.info { "Performing db migration 2: adding API key support" }
        txCmds = Array(String).new
        txCmds << "ALTER TABLE requests RENAME TO old_requests"
        txCmds << <<-SQL
          CREATE TABLE requests (
            host VARCHAR NOT NULL,
            apikey VARCHAR NOT NULL DEFAULT '',
            seen TIME NOT NULL DEFAULT CURRENT_TIMESTAMP,
            hash VARCHAR NOT NULL DEFAULT '',
            CONSTRAINT requests_pk PRIMARY KEY (host, apikey)
          )
        SQL
        txCmds << "INSERT INTO requests SELECT host, '', seen, hash FROM old_requests"
        txCmds << "DROP TABLE old_requests"
        migrate_tx txCmds
      when 3
        Log.info { "Performing db migration 3: adding an apikeycomment column" }
        migrate <<-SQL
          ALTER TABLE requests ADD COLUMN apikeycomment VARCHAR NOT NULL DEFAULT ''
        SQL
      when 4
        Log.info { "Performing db migration 4: adding an address column" }
        migrate <<-SQL
          ALTER TABLE requests ADD COLUMN address VARCHAR NOT NULL DEFAULT ''
        SQL
      when 5
        Log.info { "Performing db migration 5: adding a txid column" }
        migrate <<-SQL
          ALTER TABLE requests ADD COLUMN txid VARCHAR NOT NULL DEFAULT ''
        SQL
      when 6
        Log.info { "Performing db migration 6: adding an amount column" }
        migrate <<-SQL
          ALTER TABLE requests ADD COLUMN amount VARCHAR NOT NULL DEFAULT ''
        SQL
      when 7
        Log.info { "Performing db migration 7: adding an apikeyunittype column" }
        migrate <<-SQL
          ALTER TABLE requests ADD COLUMN apikeyunittype VARCHAR NOT NULL DEFAULT ''
        SQL
      when 8
        # Add support for per API UNIT_TYPE key rate limiting
        Log.info { "Performing db migration 8: adding API UNIT_TYPE key support" }
        txCmds = Array(String).new
        txCmds << "ALTER TABLE requests RENAME TO old_requests"
        txCmds << <<-SQL
          CREATE TABLE requests (
            host VARCHAR NOT NULL,
            apikey VARCHAR NOT NULL DEFAULT '',
            seen TIME NOT NULL DEFAULT CURRENT_TIMESTAMP,
            hash VARCHAR NOT NULL DEFAULT '',
            apikeycomment VARCHAR NOT NULL DEFAULT '',
            address VARCHAR NOT NULL DEFAULT '',
            txid VARCHAR NOT NULL DEFAULT '',
            amount VARCHAR NOT NULL DEFAULT '',
            apikeyunittype VARCHAR NOT NULL DEFAULT '',
            CONSTRAINT requests_pk PRIMARY KEY (host, apikey, apikeyunittype)
          )
        SQL
        txCmds << "INSERT INTO requests SELECT host, apikey, seen, hash, apikeycomment, " \
                  "address, txid, amount, apikeyunittype FROM old_requests"
        txCmds << "DROP TABLE old_requests"
        migrate_tx txCmds
      else
        return
      end

      @db.exec "PRAGMA main.user_version = #{version + 1}"
      migrations
    end

    def migrate(statement : String)
      Log.info { "DB statement execution summary: #{@db.exec statement}" }
    end

    def migrate_tx(txCmds : Array(String))
      begin
        tx = @db.transaction do |tx|
          txCmds.each do |cmd|
            @db.exec cmd
          end
        end
        Log.info { "DB transaction success: #{tx}" }
      rescue ex
        Log.error { "ERROR: DB transaction exception due to: #{ex}" }
      end
    end

    def on_request(context : HTTP::Server::Context) : Response
      @lastRequestTime = Time.utc
      case context.request.method
      when "POST"
        on_post(context)
      when "GET"
        on_get(context)
      else
        on_not_found
      end
    rescue error
      on_error(error)
    end

    def on_error(error : Exception)
      msg = {statusCode: 500,
             error:      HTTP::Status::INTERNAL_SERVER_ERROR.to_s,
             message:    error.to_s,
      }
      Log.debug { msg.to_json }
      {
        status: HTTP::Status::INTERNAL_SERVER_ERROR,
        body:   msg,
      }
    end

    def on_error(error : ApiException)
      body = error.response.body
      statusCode = error.response.status_code

      begin
        if blob = JSON.parse(body)
          code = blob["code"]? || "NA"
          message = blob["message"]? || body
          Log.debug { "ApiException code: #{code}, message: #{message}" }

          # Check for errors requiring wallet restart
          if code.to_s =~ /^wallet_not_responding$/
            Wallet.restartWallet
          end

          # Check for errors requiring status code intercept
          if message.to_s =~ /WrongNetwork/
            statusCode = 400
          end
        else
          message = body
        end
      rescue
        message = body
      end

      msg = {statusCode: statusCode,
             error:      HTTP::Status.from_value?(statusCode).to_s,
             message:    message.to_s,
      }

      Log.debug { msg.to_json }
      {
        status: HTTP::Status.from_value?(statusCode).as(HTTP::Status),
        body:   msg,
      }
    end

    def on_not_found
      msg = {statusCode: 404,
             error:      "Not Found",
             message:    "No URL found",
      }

      Log.debug { msg.to_json }
      {
        status: HTTP::Status::NOT_FOUND,
        body:   msg,
      }
    end

    def on_bad_asset_request
      msg = {statusCode: 400,
             error:      "Bad Request",
             message:    "The asset parameter does not validate as a ${POLICY_ID}<DELIMITER>${ASSET_NAME} " \
                         "of #{API_KEY_UNIT_POLICY_ID_LEN}, #{API_KEY_UNIT_TYPE_DELIMITER.size}, " \
                         "and 0 to #{API_KEY_UNIT_ASSET_NAME_LEN} hex characters respectively, where the " \
                         "delimiter is (in brackets): (#{API_KEY_UNIT_TYPE_DELIMITER})"
      }

      Log.debug { msg.to_json }
      {
        status: HTTP::Status::BAD_REQUEST,
        body:   msg,
      }
    end

    def on_forbidden(type : String)
      msg = {statusCode: 403,
             error:      "Forbidden",
             message:    "Anonymous #{type} Access Not Allowed: please authenticate by apiKey",
      }

      Log.debug { msg.to_json }
      {
        status: HTTP::Status::FORBIDDEN,
        body:   msg,
      }
    end

    def on_recaptcha_required
      if FAUCET_FRONTEND_URL == ""
        message = "Anonymous Access Requires Recaptcha: please request funds via the frontend and complete the recaptcha"
      else
        message = "Anonymous Access Requires Recaptcha: please request funds via the frontend and complete the recaptcha; #{FAUCET_FRONTEND_URL}"
      end
      msg = {statusCode: 403,
             error:      "Forbidden",
             message:    message,
      }

      Log.debug { msg.to_json }
      {
        status: HTTP::Status::FORBIDDEN,
        body:   msg,
      }
    end

    def on_recaptcha_failed
      msg = {statusCode: 403,
             error:      "Forbidden",
             message:    "Recaptcha verification failed",
      }

      Log.debug { msg.to_json }
      {
        status: HTTP::Status::FORBIDDEN,
        body:   msg,
      }
    end

    def on_post(context : HTTP::Server::Context) : Response
      amount = LOVELACES_TO_GIVE_ANON
      apiKey = ""
      apiKeyUnitType = "lovelace"
      apiKeyComment = "Anonymous lovelace request"
      authenticated = false

      match = context.request.path.match(%r(/send-money/([^/]+)))
      return on_not_found unless match

      address = match[1]

      # API Key takes priority over both lovelace and asset anonymous requests
      if context.request.query_params.has_key?("apiKey")
        apiKey = context.request.query_params["apiKey"]
        if API_KEYS.has_key?(apiKey)
          amount = API_KEYS[apiKey][:unitsPerTx].as(UInt64)
          authenticated = true
          apiKeyUnitType = API_KEYS[apiKey][:unitType].as(String)
          apiKeyComment = API_KEYS[apiKey][:comment].as(String)
          timeBetweenRequests = API_KEYS[apiKey][:periodPerTx].as(UInt32).seconds
        else
          apiKey = ""
        end
      end

      if !authenticated
        if context.request.query_params.has_key?("asset")
          unitType = context.request.query_params["asset"]
          unless unitType =~ /^[A-Fa-f0-9]{#{API_KEY_UNIT_POLICY_ID_LEN}}#{API_KEY_UNIT_TYPE_DELIMITER}[A-Fa-f0-9]{0,#{API_KEY_UNIT_ASSET_NAME_LEN}}$/
            return on_bad_asset_request
          end
          amount = ASSETS_TO_GIVE_ANON
          apiKeyUnitType = unitType
          apiKeyComment = "Anonymous asset request"
          timeBetweenRequests = SECS_BETWEEN_REQS_ASSETS.seconds
        else
          timeBetweenRequests = SECS_BETWEEN_REQS_ANON.seconds
        end
      end

      ipPort = context.request.remote_address
      xRealIp = context.request.headers["X-Real-IP"]?
      ip = Socket::IPAddress.new(xRealIp || "127.0.0.1", 80).address



      # Log the request details
      if authenticated
        Log.info { "Auth Request: #{apiKey}  \"#{API_KEYS[apiKey][:comment]}\"  " \
                   "UNITS_PER_TX: #{amount}  " \
                   "PERIOD_PER_TX: #{API_KEYS[apiKey][:periodPerTx]}  " \
                   "IP-PORT: #{ipPort || "NA"}  " \
                   "X-Real-IP: #{xRealIp || "NA"}" }
      else
        if apiKeyUnitType == "lovelace"
          Log.info { "Anon Request: \"lovelace\"  " \
                     "UNITS_PER_TX: #{amount}  " \
                     "PERIOD_PER_TX: #{SECS_BETWEEN_REQS_ANON}  " \
                     "IP-PORT: #{ipPort || "NA"}  " \
                     "X-Real-IP: #{xRealIp || "NA"}" }
        else
          Log.info { "Anon Asset Request: #{apiKeyUnitType}  " \
                     "UNITS_PER_TX: #{amount}  " \
                     "PERIOD_PER_TX: #{SECS_BETWEEN_REQS_ASSETS}  " \
                     "IP-PORT: #{ipPort || "NA"}  " \
                     "X-Real-IP: #{xRealIp || "NA"}" }
        end
      end

      if apiKeyUnitType == "lovelace" && !ANONYMOUS_ACCESS && !authenticated
        return on_forbidden("Lovelace")
      elsif apiKeyUnitType != "lovelace" && !ANONYMOUS_ACCESS_ASSETS && !authenticated
        return on_forbidden("Asset")
      end

      if USE_RECAPTCHA_ON_ANON && !authenticated
        if context.request.query_params.has_key?("g-recaptcha-response")
          gRecaptchaResponse = context.request.query_params["g-recaptcha-response"]
          response = Wallet.recaptchaVerify(gRecaptchaResponse, ip)
          if JSON.parse(response.body)["success"]? && JSON.parse(response.body)["success"].to_s == "true"
            Log.info { "Recaptcha Verified: true" }
          else
            Log.info { "Recaptcha Verified: false" }
            return on_recaptcha_failed
          end
        else
          Log.info { "Recaptcha Verified: not provided" }
          return on_recaptcha_required
        end
      end

      rate_limiter, ip = limit_rate(
        ip,
        timeBetweenRequests.as(Time::Span),
        apiKey,
        apiKeyComment,
        address,
        amount,
        apiKeyUnitType,
        "rateLimitOnRequest"
        )

      if rate_limiter[:allow]
        on_send_money(address,
                      amount,
                      ip,
                      apiKey,
                      apiKeyUnitType,
                      apiKeyComment)
      else
        on_too_many_requests(rate_limiter[:try_again])
      end
    end

    def on_get(context : HTTP::Server::Context) : Response
      match = context.request.path.match(%r(/metrics/?$))
      return on_not_found unless match

      on_metrics
    end

    def on_metrics : Response
      now = Time.utc
      metricsDelta = now - @lastMetricsTime

      if metricsDelta.seconds > MIN_METRICS_PERIOD || @lastMetrics == ""
        result, holdings = Account.for_wallet(FAUCET_WALLET_ID)
        assetMetrics = ""
        holdings.assets.available.each do |asset|
          assetMetrics += "cardano_faucet_metrics_asset_available{asset_name=\"#{asset.asset_name}\", " \
                          "policy_id=\"#{asset.policy_id}\"} #{asset.quantity}\n"
        end
        @lastMetricsTime = now
        @lastMetrics = "cardano_faucet_metrics_value_available #{result}\n#{assetMetrics}"
      else
        Log.debug { "Metrics were fetched #{@lastMetricsTime} with a refresh period \
                   of #{MIN_METRICS_PERIOD}s; serving previous result..." }
      end
      {
        status: HTTP::Status::OK,
        body:   @lastMetrics,
      }
    end

    def on_send_money(to_address : String,
                      amount : UInt64,
                      ip : String,
                      apiKey : String,
                      apiKeyUnitType : String,
                      apiKeyComment : String,
                     ) : Response

      result = send_funds(to_address, amount, ip, apiKey, apiKeyUnitType, apiKeyComment)
      {
        status: HTTP::Status::OK,
        body:   result,
      }
    end

    def on_too_many_requests(try_again : Time) : Response
      delta = (try_again - @lastRequestTime).total_seconds.to_i
      msg = {statusCode: 429,
             error:      "Too Many Requests",
             message:    "Try again in #{delta} seconds",
             retryAfter: try_again.to_utc,
      }
      Log.debug { msg.to_json }
      {
        status: HTTP::Status::TOO_MANY_REQUESTS,
        body:   msg,
      }
    end

    def real_ip_port(header : String) : String
      "#{header}:443"
    end

    def real_ip_port(header : Nil) : Nil
    end

    def limit_rate(ip : Nil,
                   timeBetweenRequests : Time::Span,
                   apiKey : String,
                   apiKeyComment : String,
                   address : String,
                   amount : UInt64,
                   apiKeyUnitType : String,
                   txId : String
                  ) : Tuple(NamedTuple(time: Time, allow: Bool, try_again: Time), String)

      return({time:      @lastRequestTime,
              allow:     false,
              try_again: @lastRequestTime + timeBetweenRequests,
             },
             "")
    end

    def limit_rate(ip : String,
                   timeBetweenRequests : Time::Span,
                   apiKey : String,
                   apiKeyComment : String,
                   address : String,
                   amount : UInt64,
                   apiKeyUnitType : String,
                   txId : String
                  ) : Tuple(NamedTuple(time: Time, allow: Bool, try_again: Time), String)

      allow_after = @lastRequestTime - timeBetweenRequests

      found = nil

      select_seen(ip, address, apiKey, allow_after, apiKeyUnitType) do |rs|
        rs.each do
          seen = rs.read(Time)
          found = {
            time:      seen,
            allow:     false,
            try_again: seen + timeBetweenRequests,
          }
        end
      end

      if found
        return(found.not_nil!, ip)
      end

      unless RATE_LIMIT_ON_SUCCESS
        @db.exec(<<-SQL, ip, apiKey, @lastRequestTime, @settings.genesis_block_hash, apiKeyComment, address, txId, amount.to_s, apiKeyUnitType)
          INSERT OR REPLACE INTO requests VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?)
        SQL
      end

      return({time:      @lastRequestTime,
              allow:     true,
              try_again: allow_after,
             },
             ip)
    end

    def select_seen(ip, address, apiKey, allow_after, apiKeyUnitType, &block : DB::ResultSet -> Nil)
      @db.query(<<-SQL, ip, address, apiKey, allow_after, @settings.genesis_block_hash, apiKeyUnitType, &block)
        SELECT seen FROM requests
          WHERE (host = ? OR address = ?)
          AND apikey = ?
          AND seen > ?
          AND hash = ?
          AND apikeyunittype = ?
          ORDER BY seen DESC
          LIMIT 1
      SQL
    end

    def sh(cmd, args)
      raise "Failed to execute #{cmd} #{args.join(" ")}" unless system(cmd, args)
    end

    def sh!(cmd)
      result = `#{cmd}`
      raise "Failed to execute #{cmd}" unless $?.success?
      result.strip
    end

    def send_funds(address : String,
                   amount : UInt64,
                   ip : String,
                   apiKey : String,
                   apiKeyUnitType : String,
                   apiKeyComment : String,
                  ) : SendFundsResult


      # Setup the transaction basics
      source_account_value, holdings = Account.for_wallet(FAUCET_WALLET_ID)
      path = USE_BYRON_WALLET ? "#{WALLET_API}/byron-wallets/#{FAUCET_WALLET_ID}/transactions" : "#{WALLET_API}/wallets/#{FAUCET_WALLET_ID}/transactions"
      minLovelace = @settings.minimum_utxo_value.quantity

      # Make an estimate on the cost of the transaction
      if apiKeyUnitType == "lovelace"
        tx_fees = Fees.for_tx(FAUCET_WALLET_ID, address, amount, apiKeyUnitType)
        amount_with_fees = amount + tx_fees
      else
        # apiKeyUnitType has already been regex validated as ${POLICY_ID}<DELIMITER>${ASSET_NAME}
        policyId = apiKeyUnitType[0, API_KEY_UNIT_POLICY_ID_LEN]
        assetName = apiKeyUnitType[API_KEY_UNIT_POLICY_ID_LEN + API_KEY_UNIT_TYPE_DELIMITER.size, apiKeyUnitType.size]

        # Check the asset inventory is sufficient for the request
        assetAvailable = 0
        holdings.assets.available.each do |asset|
          if asset.policy_id == policyId && asset.asset_name = assetName
            assetAvailable = asset.quantity
            break
          end
        end
        if assetAvailable < amount
          Log.error { "Not enough asset in faucet account, only #{assetAvailable} #{apiKeyUnitType} left" }
          raise "Not enough funds in faucet account, only #{assetAvailable} #{apiKeyUnitType} left"
        else
          Log.info { "Faucet asset: { \"pre-tx\": \"#{assetAvailable}\", " \
                     "\"post-tx\": \"#{assetAvailable - amount}\", " \
                     "\"asset\": \"#{apiKeyUnitType}\" }" }
        end

        # Improve this with an asset transaction fee estimator once available
        tx_fees = Fees.for_tx(FAUCET_WALLET_ID, address, amount, apiKeyUnitType)
        amount_with_fees = (minLovelace * 10) + tx_fees
      end

      # Error if not enough funds available for the estimated cost of the transaction
      if source_account_value < amount_with_fees
        Log.error { "Not enough funds in faucet account, only #{source_account_value} left" }
        raise "Not enough funds in faucet account, only #{source_account_value} left"
      else
        Log.info { "Faucet funds: { \"pre-tx\": \"#{source_account_value}\", " \
                   "\"post-tx\": \"#{source_account_value - amount_with_fees}\" }" }
      end

      # If we want to add a faucet Tx count metric
      # source_tx_counter = Txs.for_wallet(FAUCET_WALLET_ID)

      # Craft the body of the request depending on whether ADA in lovelace or an asset is to be transferred
      if apiKeyUnitType == "lovelace"
        body = %({"payments":[{"address":"#{address}","amount":{"quantity":#{amount},"unit":"lovelace"}}],"passphrase":"#{SECRET_PASSPHRASE}"})
      else
        payments = %({"payments":[{"address":"#{address}","amount":{"quantity":0,"unit":"lovelace"})
        assets = %("assets":[{"policy_id":"#{policyId}","asset_name":"#{assetName}","quantity":#{amount}}]}])
        passphrase = %("passphrase":"#{SECRET_PASSPHRASE}"})
        body = %(#{payments},#{assets},#{passphrase})
      end

      # Submit the Tx and obtain basic debug info
      Log.debug { "Performing send; curl equivalent:" }
      Log.debug { "curl -vX POST #{path} -H 'Content-Type: application/json; charset=utf-8' -d '#{body}' --http1.1" }
      response = Wallet.apiPost(path, body)
      id = JSON.parse(response.body)["id"].as_s

      # If an asset is being transferred, obtain the minimum lovelace that was required
      # Move this to an estimate pre-transaction once an estimate endpoint supports this
      if apiKeyUnitType != "lovelace"
        outputs = JSON.parse(response.body)["outputs"].as_a
        outputs.each do |output|
          if output["address"] == address
            minLovelace = output["amount"]["quantity"].as_i64
            break
          end
        end
      end

      msg = {
        success:      response.success?,
        amount:       amount,
        unit:         apiKeyUnitType,
        fee:          tx_fees,
        minLovelace:  minLovelace,
        txid:         id,
      }
      Log.info { msg.to_json }

      if RATE_LIMIT_ON_SUCCESS
        @db.exec(<<-SQL, ip, apiKey, @lastRequestTime, @settings.genesis_block_hash, apiKeyComment, address, id, amount.to_s, apiKeyUnitType)
          INSERT OR REPLACE INTO requests VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?)
        SQL
      end

      msg
    end
  end
end
