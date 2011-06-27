% Handler for registered client connections
-module(webbus_client_handler).
-behaviour(gen_event).
-export([init/1, handle_event/2, handle_call/2, handle_info/2, terminate/2, code_change/3]).
-include_lib("../deps/socketio/include/socketio.hrl").
-define(HASH_SIZE, 160).
-record(state, {peer_addr, client_pid = undefined}).

init([Request]) ->
    PeerAddr = misultin_ws:get(peer_addr, Request),
    io:format("*** ~p connected~n", [PeerAddr]),
    {ok, #state{peer_addr = PeerAddr}}.
    
handle_event({message, Client, #msg{content = Content}}, State) ->
    case parse_command(Content) of
        {ok, Command, Params, Ref} ->
            handle_command(Client, Command, Params, Ref, State);
        error ->
            bad_request(Client, Content),
            {ok, State}
    end.
    
handle_call(_, State) ->
    {reply, ok, State}.

handle_info(_, State) ->
    {ok, State}.

terminate(_Reason, State) ->
    io:format("*** ~p disconnected~n", [State#state.peer_addr]),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
    
parse_command([{Command, Params}]) ->
    {ok, Command, Params, undefined};
parse_command([{Command, Params}, {<<"ref">>, _} = Ref]) ->
    {ok, Command, Params, Ref};
parse_command([{<<"ref">>, _} = Ref, {Command, Params}]) ->
    {ok, Command, Params, Ref};
parse_command(_) ->
    error.

send_response(Client, Command, Payload, Ref) ->
    Response = case Ref of
        {<<"ref">>, _} -> [Ref];
        undefined      -> []
    end,
    Content = [{Command, Payload}|Response],
    socketio_client:send(Client, #msg{json = true, content = Content}).

handle_command(Client, Command, Params, Ref, #state{client_pid=Pid} = State) when is_pid(Pid) ->
    registered_command(Client, Command, Params, Ref, State),
    {ok, State};
handle_command(Client, <<"register">>, Params, _Ref, State) ->
    register_client(Client, Params),
    {ok, State};
handle_command(Client, Command, Params, Ref, State) ->
    not_found(Client, Command, Params, Ref),
    {ok, State}.
    
registered_command(Client, Command, Params, Ref, _State) ->
    not_found(Client, Command, Params, Ref).

register_client(_Client, Params) ->
    Tripcode = case proplists:get_value(<<"tripcode">>, Params) of
        undefined ->
            crypto:rand_bytes(?HASH_SIZE); % Generate a random tripcode
        Bin when is_binary(Bin) ->
            Bin
    end,

    io:format("Registering with tripcode: ~p~n", [Tripcode]).
    
bad_request(Client, Content) ->
    Response = [{<<"error">>, [
        {<<"code">>, 400},
        {<<"text">>, <<"Bad request">>},
        {<<"message">>, Content}
    ]}],
    socketio_client:send(Client, #msg{json = true, content = Response}).
    
not_found(Client, Command, Params, Ref) ->
    send_response(Client, <<"error">>, [
        {<<"code">>, 404},
        {<<"text">>, <<"Unknown command">>},
        {<<"command">>, Command}, 
        {<<"params">>, Params}
    ], Ref).