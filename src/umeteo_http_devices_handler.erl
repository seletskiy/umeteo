%% @author Stanislav Seletskiy <s.seletskiy@gmail.com>
%% @doc Devices HTTP Handler.
-module(umeteo_http_devices_handler).
-created('Date: 04/10/2013').
-created_by('Stanislav Seletskiy <s.seletskiy@gmail.com>').

-export([
    init/3,
    allowed_methods/2,
    content_types_provided/2,
    charsets_provided/2,
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

get_json(Req, State) ->
    {HostUrl, _} = cowboy_req:host_url(Req),
    {Path, _} = cowboy_req:path(Req),
    BaseUrl = <<HostUrl/binary, Path/binary>>,
    {jsonx:encode([{devices,
        prepare_devices(BaseUrl,
            ets:match(umeteo_devices, '$1'))}]),
        Req, State}.

prepare_devices(BaseUrl, Devices) ->
    prepare_devices(BaseUrl, Devices, []).

prepare_devices(_BaseUrl, [], Result) ->
    lists:reverse(Result);

prepare_devices(BaseUrl, [[{DevId, Timestamp, Status}] | Tail], Result) ->
    EncodedStatus = case Status of
        ok ->
            <<"ok">>;
        {error, Error} ->
            [{error, iolist_to_binary(io_lib:format("~999p", [Error]))}]
    end,
    ValuesUrl = <<BaseUrl/binary, "/", DevId/binary, "/values">>,
    prepare_devices(BaseUrl, Tail, [[
        {id, DevId},
        {age, timestamp() - Timestamp},
        {status, EncodedStatus},
        {links, [{values, ValuesUrl}]}] | Result]).

timestamp() ->
    {Mega, Secs, Micro} = now(),
    Mega * 1000000 + Secs + Micro / 1000000.
