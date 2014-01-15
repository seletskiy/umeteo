%% @author Stanislav Seletskiy <s.seletskiy@gmail.com>
%% @doc Main HTTP Handler.
-module(umeteo_http_values_handler).
-created('Date: 04/10/2013').
-created_by('Stanislav Seletskiy <s.seletskiy@gmail.com>').

-export([
    init/3,
    allowed_methods/2,
    content_types_provided/2,
    charsets_provided/2,
    resource_exists/2,
    get_json/2
]).

init(_Transport, _Req, []) ->
    {upgrade, protocol, cowboy_rest}.

allowed_methods(Req, State) ->
    {[<<"GET">>], Req, State}.

content_types_provided(Req, State) ->
    {[
        {{<<"application">>, <<"json">>, []}, get_json}
    ], Req, State}.

charsets_provided(Req, State) ->
    {[<<"utf-8">>], Req, State}.

resource_exists(Req, State) ->
    {DevId, _} = cowboy_req:binding(dev_id, Req),
    case ets:lookup(umeteo_devices, DevId) of
        [] ->
            {false, Req, State};
        [DevInfo] ->
            Req2 = cowboy_req:set_meta(device, DevInfo, Req),
            {true, Req2, State}
    end.

get_json(Req, State) ->
    {{DevId, Timestamp, Status, ConnectedTo}, _} = cowboy_req:meta(device, Req),
    EncodedStatus = case Status of
        ok ->
            <<"ok">>;
        offline ->
            <<"offline">>;
        {error, Error} ->
            [{error, iolist_to_binary(io_lib:format("~999p", [Error]))}]
    end,
    {Host, Port} = ConnectedTo,
    {jsonx:encode([
        {values, prepare_values(DevId)},
        {meta, [
            {connected_to, [
                {host, iolist_to_binary(Host)}, {port, Port}
            ]},
            {age, timestamp() - Timestamp},
            {status, EncodedStatus}]}]),
        Req, State}.

prepare_values(DevId) ->
    prepare_values(DevId, ets:match(umeteo_values,
        {{DevId, '$1'}, '$2', '$3'}), []).

prepare_values(_DevId, [], Result) ->
    lists:reverse(Result);

prepare_values(DevId, [[ChanId, Timestamp, Value] | Tail], Result) ->
    [{_, ChanInfo}] = ets:lookup(umeteo_channels, {DevId, ChanId}),
    ChanInfo2 = lists:keydelete(mv_type, 1, ChanInfo),
    ChanInfo3 = lists:keydelete(channel, 1, ChanInfo2),
    ChanInfo4 = lists:keystore(id, 1, ChanInfo3,
        {id, ChanId}),
    ChanInfo5 = lists:keystore(age, 1, ChanInfo4,
        {age, timestamp() - Timestamp}),
    ChanInfo6 = lists:keystore(value, 1, ChanInfo5,
        {value, Value}),
    prepare_values(DevId, Tail, [ChanInfo6 | Result]).

timestamp() ->
    {Mega, Secs, Micro} = now(),
    Mega * 1000000 + Secs + Micro / 1000000.
