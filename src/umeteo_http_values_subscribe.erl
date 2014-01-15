%% @author Stanislav Seletskiy <s.seletskiy@gmail.com>
%% @doc WebSocket Protocol for device values.
-module(umeteo_http_values_subscribe).
-created('Date: 15/01/2014').
-created_by('Stanislav Seletskiy <s.seletskiy@gmail.com>').

-export([
    init/3,
    websocket_init/3,
    websocket_info/3,
    websocket_handle/3,
    websocket_terminate/3
]).

init({tcp, http}, _Req, _Opts) ->
    {upgrade, protocol, cowboy_websocket}.


websocket_init(_TransportName, Req, _Opts) ->
    pg2:join(umeteo_listeners, self()),
    {ok, Req, nil}.

websocket_handle(_Msg, Req, State) ->
    {ok, Req, State}.

websocket_info({update, {ChanId, Value}}, Req, State) ->
    {reply, {text, jsonx:encode([
        {id, ChanId},
        {value, Value}
    ])}, Req, State};
websocket_info(_Info, Req, State) ->
    {ok, Req, State}.

websocket_terminate(_Reason, _Req, _State) ->
    ok.
