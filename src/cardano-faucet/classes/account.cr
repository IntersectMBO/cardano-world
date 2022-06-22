module Cardano
  class Account
    def self.for_wallet(walletId)
      # value = JSON.parse(`cardano-wallet wallet get #{walletId}`)["balance"]["available"]["quantity"].as_i64

      path = USE_BYRON_WALLET ? "#{WALLET_API}/byron-wallets/#{walletId}" : "#{WALLET_API}/wallets/#{walletId}"
      Log.debug { "Fetching available wallet value; curl equivalent:" }
      Log.debug { "curl -v #{path}" }
      response = Wallet.apiGet(path)
      value = JSON.parse(response.body)["balance"]["available"]["quantity"].as_i64
      holdings = Holding.from_json(response.body)
      return value.not_nil!, holdings
    end

    class Holding
      JSON.mapping(
        assets: { type: Asset },
      )
    end

    class Asset
      JSON.mapping(
        total: { type: Array(Inventory) },
        available: { type: Array(Inventory) },
      )
    end

    class Inventory
      JSON.mapping(
        asset_name: String,
        quantity: UInt64,
        policy_id: String,
      )
    end
  end
end
