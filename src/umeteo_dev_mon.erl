%% @author Stanislav Seletskiy <s.seletskiy@gmail.com>
%% @doc Device Monitor.
-module(umeteo_dev_mon).
-created('Date: 04/10/2013').
-created_by('Stanislav Seletskiy <s.seletskiy@gmail.com>').

-behaviour(gen_server).

-export([
    start_link/5
]).

-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

-record(state, {
    master,
    device,
    bus,
    interval,
    id
}).

start_link(Id, Host, Port, Timeout, Interval) ->
    gen_server:start_link(?MODULE, [Id, Host, Port, Timeout, Interval], []).

%% @hidden
init([Id, Host, Port, Timeout, Interval]) ->
    sl:info("~p - connecting to ~s:~B", [Id, Host, Port]),
    {ok, BusConnection} = umb_bus:connect(Id, umb_transport_tcp_nokeep, [
        Host, Port, [
            {inet, [{send_timeout, Timeout}]},
            {recv_timeout, Timeout}
        ]]),
    link(BusConnection),
    gen_server:cast(self(), {start, Interval}),
    BinId = atom_to_binary(Id, utf8),
    ets:insert(umeteo_devices, {BinId, timestamp(), offline, {Host, Port}}),
    {ok, #state{
        master = umb_device:new(15, 1),
        device = umb_device:new(2, 1),
        bus = BusConnection,
        interval = Interval,
        id = BinId}}.

%% @hidden
handle_call(_Message, _From, State) ->
    {noreply, State}.

%% @hidden
handle_cast({start, Interval}, State) ->
    case read_channels_list(State) of
        ok ->
            sl:info("~s - starting data retrieval", [State#state.id]),
            timer:send_after(Interval, self(), {refresh, Interval}),
            {noreply, State};
        _Error ->
            sl:crit("~s - can not read channels list, starting failover", [
                State#state.id]),
            timer:sleep(State#state.interval),
            {stop, failover, State}
    end;
handle_cast(_Message, State) ->
    {noreply, State}.

%% @hidden
handle_info({refresh, Interval}, State) ->
    Status = request_channels_value(State),
    ets:update_element(umeteo_devices, State#state.id, [
        {2, timestamp()},
        {3, Status}]),
    case Status of
        ok ->
            timer:send_after(Interval, self(), {refresh, Interval}),
            {noreply, State};
        {error, {source_error, closed, _}} ->
            sl:crit("no connection to server, starting failover"),
            {stop, failover, State}
    end;

handle_info(_Info, State) ->
    {noreply, State}.

%% @hidden
terminate(_Reason, _State) ->
    ok.

%% @hidden
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

request(Request, State) ->
    Result = umb_bus:request(
        State#state.bus, State#state.master, State#state.device,
        Request),
    case Result of
        {ok, Frame} ->
            umb_request:payload(Frame);
        Error ->
            sl:error("error while request <~999p>: ~999p", [Request, Error]),
            Error
    end.

read_channels_list(State) ->
    sl:info("~s - reading channels list", [
        State#state.id]),
    case request(umb_request:info_cnl_num(), State) of
        {ok, {_, ChBlocks}} ->
            sl:info("~s - found ~B channel blocks", [
                State#state.id, ChBlocks]),
            read_channels_info(ChBlocks, State);
        Error ->
            sl:error("~s - error while requesting channels list", [
                State#state.id]),
            Error
    end.

read_channels_info(BlockNumber, State) ->
    read_channels_info(BlockNumber, BlockNumber, State).

read_channels_info(0, _Total, _State) ->
    ok;

read_channels_info(Current, Total, State) ->
    case request(umb_request:info_cnl_list(Total - Current), State) of
        {ok, List} ->
            sl:debug("~s - processing ~B channels: ~999p", [
                State#state.id, length(List), List]),
            process_channels_list(List, State),
            read_channels_info(Current - 1, Total, State);
        Error ->
            sl:error("~s - error while requesting channels", [State#state.id]),
            Error
    end.

process_channels_list([], _State) ->
    ok;

process_channels_list([ChanId | Tail], State) ->
    case ets:lookup(umeteo_channels, {State#state.id, ChanId}) of
        [] ->
            sl:debug("~s - processing channel <~B>", [
                State#state.id, ChanId]),
            case request(umb_request:info_cnl_full(ChanId), State) of
                 {ok, Info} ->
                    ets:insert(umeteo_channels, {{State#state.id, ChanId},
                        Info});
                Error ->
                    sl:error("~s - error while processing channel ~B", [
                        State#state.id, ChanId]),
                    Error
            end;
        [_Cached] ->
            ok
    end,
    process_channels_list(Tail, State).

request_channels_value(State) ->
    request_channels_value(
        lists:map(fun hd/1,
            ets:match(umeteo_channels, {{State#state.id, '$1'}, '_'})),
        State).

request_channels_value([], _) ->
    ok;

request_channels_value(Channels, State) ->
    {Slice20, Tail} = lists:split(min(20, length(Channels)), Channels),
    Request = umb_request:multichannel(
        lists:map(fun umb_request:online_data/1, Slice20)),
    case request(Request, State) of
        {ok, Payload} ->
            process_channels_value(State#state.id, Slice20, Payload),
            request_channels_value(Tail, State);
        Error ->
            Error
    end.

process_channels_value(_, [], []) ->
    ok;

process_channels_value(DevId, [ChanId | ChanTail], [Value | ValuesTail]) ->
    ets:insert(umeteo_values, {{DevId, ChanId}, timestamp(), Value}),
    notify_listeners(pg2:get_members(umeteo_listeners), {ChanId, Value}),
    process_channels_value(DevId, ChanTail, ValuesTail).

notify_listeners([], _Data) ->
    ok;
notify_listeners([Pid | Tail], Data) ->
    Pid ! {update, Data},
    notify_listeners(Tail, Data).

timestamp() ->
    {Mega, Secs, Micro} = now(),
    Mega * 1000000 + Secs + Micro / 1000000.
