module Cardano
  class Settings
    JSON.mapping(
      genesis_block_hash: String,
      minimum_utxo_value: { type: MinUtxoValue },
    )

    def self.get
      # from_json(`cardano-wallet network parameters`)

      path = "#{WALLET_API}/network/parameters"
      Log.debug { "Fetching network parameters; curl equivalent:" }
      Log.debug { "curl -v #{path}" }
      response = Wallet.apiGet(path)
      networkParameters = from_json(response.body)
      if networkParameters.minimum_utxo_value.unit != "lovelace"
        raise "minimum_utxo_value is no longer lovelace"
      end
      networkParameters
    end

    class MinUtxoValue
      JSON.mapping(
        quantity: Int64,
        unit: String,
      )
    end
  end
end
