module Cardano
  class Wallet
    def self.recaptchaVerify(recaptchaResponse, ip)
      params = HTTP::Params.encode({"secret" => RECAPTCHA_SECRET,
                                    "response" => recaptchaResponse,
                                    "remoteip" => ip })
      response = HTTP::Client.post(RECAPTCHA_URI + "?" + params, RECAPTCHA_HEADER)
      result = response.body
      statusCode = response.status_code
      statusMessage = response.status_message
      Log.debug { "reCaptcha response: #{response}" }
      Log.debug { "reCaptcha submitted ip: #{ip}" }
      if response.success?
        Log.debug { "statusCode: #{statusCode}" }
        Log.debug { "statusMessage: #{statusMessage}" }
        Log.debug { "Result: #{result.to_s.delete('\n')}" }
      else
        Log.error { "statusCode: #{statusCode}" }
        Log.error { "statusMessage: #{statusMessage}" }
        Log.error { "Result: #{result.to_s.delete('\n')}" }
        apiRaise response
      end
      return response
    end

    def self.apiPost(path, body)
      client = HTTP::Client.new(API_URI)
      response = client.post(path, HEADERS, body)
      result = response.body
      statusCode = response.status_code
      statusMessage = response.status_message
      Log.debug { "response: #{response}" }
      if response.success?
        Log.debug { "statusCode: #{statusCode}" }
        Log.debug { "statusMessage: #{statusMessage}" }
        Log.debug { "Result: #{result}" }
      else
        Log.error { "statusCode: #{statusCode}" }
        Log.error { "statusMessage: #{statusMessage}" }
        Log.error { "Result: #{result}" }
        apiRaise response
      end
      return response
    end

    def self.apiGet(path)
      client = HTTP::Client.new(API_URI)
      response = client.get(path)
      result = response.body
      statusCode = response.status_code
      statusMessage = response.status_message
      Log.debug { "response: #{response}" }
      if response.success?
        Log.debug { "statusCode: #{statusCode}" }
        Log.debug { "statusMessage: #{statusMessage}" }
        Log.debug { "Result: #{result}" }
      else
        Log.error { "statusCode: #{statusCode}" }
        Log.error { "statusMessage: #{statusMessage}" }
        Log.error { "Result: #{result}" }
        apiRaise response
      end
      return response
    end

    def self.apiRaise(response)
      raise ApiException.new(response)
    end

    def self.restartWallet
      IO_CMD_OUT.clear
      IO_CMD_ERR.clear
      Log.debug { "Found a cardano-wallet code event requiring cardano-wallet systemd service restart -- restarting..." }
      cmd = "/run/wrappers/bin/sudo /run/current-system/sw/bin/systemctl restart cardano-wallet.service"
      result = Process.run(cmd, output: IO_CMD_OUT, error: IO_CMD_ERR, shell: true)
      Log.debug { "Cardano-wallet restart result:\nRestart success: #{result.success?}\nSTDOUT: #{IO_CMD_OUT}\nSTDERR: #{IO_CMD_ERR}" }
    end
  end
end
