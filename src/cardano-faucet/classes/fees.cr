module Cardano
  class Fees
    def self.for_tx(walletId, dest_addr, amount, unitType)
      # fees = JSON.parse(`cardano-wallet transaction fees #{walletId} --payment #{amount}@#{dest_addr}`)["amount"]["quantity"].as_i

      path = USE_BYRON_WALLET ? "#{WALLET_API}/byron-wallets/#{walletId}/payment-fees" : "#{WALLET_API}/wallets/#{walletId}/payment-fees"
      if unitType == "lovelace"
        body = %({"payments":[{"address":"#{dest_addr}","amount":{"quantity":#{amount},"unit":"lovelace"}}]})
      else
        # apiKeyUnitType has already been regex validated as ${POLICY_ID}<DELIMITER>${ASSET_NAME}
        policyId = unitType[0, API_KEY_UNIT_POLICY_ID_LEN]
        assetName = unitType[API_KEY_UNIT_POLICY_ID_LEN + API_KEY_UNIT_TYPE_DELIMITER.size, unitType.size]
        payments = %({"payments":[{"address":"#{dest_addr}","amount":{"quantity":0,"unit":"lovelace"})
        assets = %("assets":[{"policy_id":"#{policyId}","asset_name":"#{assetName}","quantity":#{amount}}]}]})
        body = %(#{payments},#{assets})
      end
      Log.debug { "Fetching transaction fee estimate; curl equivalent:" }
      Log.debug { "curl -vX POST #{path} -H 'Content-Type: application/json; charset=utf-8' -d '#{body}' --http1.1" }
      response = Wallet.apiPost(path, body)
      fees = JSON.parse(response.body)["estimated_min"]["quantity"].as_i64
      return fees.not_nil!
    end
  end
end
