%% @author Stanislav Seletskiy <s.seletskiy@gmail.com>
%% @doc Upstream manager for device.
-module(umeteo_dev_upstream).
-created('Date: 13/01/2014').
-created_by('Stanislav Seletskiy <s.seletskiy@gmail.com>').

-export([
    start_link/4
]).

start_link(Id, Upstream, Timeout, Interval) ->
    LastServer = case ets:lookup(umeteo_devices, Id) of
        [] -> nil;
        [{_, _, _, L}] -> L
    end,
    Random = random:uniform(),
    sl:debug("~s - random seed is ~.2f", [Id, Random]),
    {NewUpstream, TotalWeight} = normalize_upstream(Upstream, LastServer),
    {NewWeight, NewHost, NewPort} = choose_server(
        Id, NewUpstream, TotalWeight, Random),
    sl:info("~s - starting connection to ~s:~B (weight ~.2f)",
        [Id, NewHost, NewPort, NewWeight]),
    umeteo_dev_mon:start_link(Id, NewHost, NewPort, Timeout, Interval).

normalize_upstream(Upstream, LastServer) ->
    normalize_upstream(Upstream, LastServer, 0, []).

normalize_upstream([], _, TotalWeight, NewUpstream) ->
    {lists:reverse(NewUpstream), TotalWeight};

normalize_upstream([{_, Host, Port} | Tail], {Host, Port}, TotalWeight,
        NewUpstream) ->
    normalize_upstream(Tail, nil, TotalWeight, NewUpstream);

normalize_upstream([{Weight, Host, Port} | Tail], _, TotalWeight,
        NewUpstream) ->
    normalize_upstream(Tail, nil, TotalWeight + Weight,
        [{Weight, Host, Port} | NewUpstream]).

choose_server(Id, [{Weight, Host, Port} | _], TotalWeight, Random)
        when Random =< Weight / TotalWeight ->
    sl:debug("~s - chosen ~s:~B: ~.2f <= ~.2f", [
        Id, Host, Port, Random, Weight / TotalWeight]),
    {Weight, Host, Port};

choose_server(Id, [{Weight, Host, Port} | Tail], TotalWeight, Random) ->
    sl:debug("~s - skipped ~s:~B: ~.2f > ~.2f", [
        Id, Host, Port, Random, Weight / TotalWeight]),
    choose_server(Id, Tail, TotalWeight, Random - Weight / TotalWeight).
