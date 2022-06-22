module Cardano
  class Txs
    def self.for_wallet(walletId)
      # counter = JSON.parse(`cardano-wallet transaction list #{walletId}`)

      path = USE_BYRON_WALLET ? "#{WALLET_API}/byron-wallets/#{walletId}/transactions" : "#{WALLET_API}/wallets/#{walletId}/transactions"
      Log.debug { "Fetching wallet transaction count; curl equivalent:" }
      Log.debug { "curl -v #{path} | jq '. | length'" }
      response = Wallet.apiGet(path)
      counter = JSON.parse(response.body)
      return counter.not_nil!.size
    end
  end
end
