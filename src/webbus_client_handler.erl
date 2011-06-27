% Handler for registered client connections
-module(webbus_client_handler).
-behaviour(gen_event).
-export([init/1, handle_event/2, handle_call/2, handle_info/2, terminate/2, code_change/3]).
-include_lib("../deps/socketio/include/socketio.hrl").
-define(HASH_SIZE, 160).
-record(state, {peer_addr}).

init([Request]) ->
    PeerAddr = misultin_ws:get(peer_addr, Request),
    io:format("*** ~p connected~n", [PeerAddr]),
    {ok, #state{peer_addr = PeerAddr}}.
    
handle_event({message, Client, #msg{content = [{Command, Params}]}}, State) ->
    handle_command(Command, Client, Params, State),
    {ok, State};
handle_event({message, Client, #msg{content = Content}}, State) ->
    Response = [{<<"error">>, [
        {<<"code">>, 400},
        {<<"text">>, <<"Bad request">>},
        {<<"message">>, Content}
    ]}],
    socketio_client:send(Client, #msg{json = true, content = Response}),
    {ok, State};
handle_event(_Event, State) ->
    %% FIXME: Uhh, log unknown event or something?
    {ok, State}.
    
handle_call(_, State) ->
    {reply, ok, State}.

handle_info(_, State) ->
    {ok, State}.

terminate(_Reason, State) ->
    io:format("*** ~p disconnected~n", [State#state.peer_addr]),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
    
handle_command(<<"register">>, Client, Params, State) ->
    register_client(Client, Params),
    {ok, State};
handle_command(Command, Client, Params, State) ->
    Response = [{<<"error">>, [
        {<<"code">>, 404},
        {<<"text">>, <<"Unknown command">>},
        {<<"command">>, Command}, 
        {<<"params">>, Params}
    ]}],
    socketio_client:send(Client, #msg{json = true, content = Response}),
    {ok, State}.

register_client(_Client, Params) ->
    Tripcode = case proplists:get_value(<<"tripcode">>, Params) of
        undefined ->
            crypto:rand_bytes(?HASH_SIZE); % Generate a random tripcode
        Bin when is_binary(Bin) ->
            Bin
    end,

    io:format("Registering with tripcode: ~p~n", [Tripcode]).