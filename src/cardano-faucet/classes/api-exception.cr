module Cardano
  class ApiException < Exception
    getter response

    @response : HTTP::Client::Response

    def initialize(@response)
    end
  end
end
