-module(webbus_http_handler).
-export([handle_request/3]).

handle_request('GET', [], Req) ->
    Ebin = filename:dirname(code:which(?MODULE)),
    Public = filename:absname(filename:join([Ebin, "..", "public"])),
    Path = filename:join([Public, "index.html"]),
    Req:file(Path);
handle_request(_Method, _Path, Req) ->
    Req:respond(404).