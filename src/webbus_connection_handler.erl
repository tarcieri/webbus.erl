% Socket.io connection handler
-module(webbus_connection_handler).
-behaviour(gen_event).
-export([init/1, handle_event/2, handle_call/2, handle_info/2, terminate/2, code_change/3]).

init([]) ->
    {ok, undefined}.

handle_event({client, Pid}, State) ->
    io:format("Connected: ~p~n", [Pid]),
    EventMgr = socketio_client:event_manager(Pid),
    Request =  socketio_client:request(Pid),
    
    ok = gen_event:add_handler(EventMgr, webbus_client_handler, [Request]),
    {ok, State};
handle_event({disconnect, Pid}, State) ->
    io:format("Disconnected: ~p~n",[Pid]),
    {ok, State}.
    
handle_call(_, State) ->
    {reply, ok, State}.

handle_info(_, State) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.