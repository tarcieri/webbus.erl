% Calculate hashes for client IDs from a given tripcode
-module(webbus_hash).
-export([data/1]).
-define(SERVER_KEY, <<"lol imma insecure key">>).

data(Data) ->
    <<IntHash:160/integer>> = crypto:sha([?SERVER_KEY, Data]),
    [StringHash] = io_lib:format("~40.16.0b", [IntHash]),
    list_to_binary(StringHash).