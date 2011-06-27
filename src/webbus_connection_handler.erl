% Socket.io connection handler
-module(webbus_connection_handler).
-behaviour(gen_event).
-export([init/1, handle_event/2, handle_call/2, handle_info/2, terminate/2, code_change/3]).
-include_lib("../deps/socketio/include/socketio.hrl").
-define(HASH_SIZE, 160).

%% gen_event
init([]) ->
    {ok, undefined}.

handle_event({client, Pid}, State) ->
    io:format("Connected: ~p~n", [Pid]),
    EventMgr = socketio_client:event_manager(Pid),
    ok = gen_event:add_handler(EventMgr, ?MODULE,[]),
    {ok, State};
handle_event({disconnect, Pid}, State) ->
    io:format("Disconnected: ~p~n",[Pid]),
    {ok, State};
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

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
%%    

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
    
register_client(Client, Params) ->
    Tripcode = case proplists:get_value(<<"tripcode">>, Params) of
        undefined ->
            crypto:rand_bytes(?HASH_SIZE); % Generate a random tripcode
        Bin when is_binary(Bin) ->
            Bin
    end,
    
    % FIXME: At some point snag the cleint IP out of here
    % Request = socketio_client:request(Client),

    io:format("Registering with tripcode: ~p~n", [Tripcode]).