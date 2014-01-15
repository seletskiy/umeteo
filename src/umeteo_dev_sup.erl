%% @author Stanislav Seletskiy <s.seletskiy@gmail.com>
%% @doc umeteo app supervisor.
-module(umeteo_dev_sup).
-created('Date: 02/10/2013').
-created_by('Stanislav Seletskiy <s.seletskiy@gmail.com>').
-behaviour(supervisor).

-export([
    start_link/1,
    init/1
]).

start_link(Config) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, [Config]).

init([Config]) ->
    ets:new(umeteo_values, [public, named_table]),
    ets:new(umeteo_devices, [public, named_table]),
    ets:new(umeteo_channels, [public, named_table]),
    sl:info("reading devices conf from <~s>", [Config]),
    random:seed(now()),
    {ok, DevList} = file:consult(Config),
    {ok, {{one_for_one, 5, 60},
        lists:map(fun(DevInfo) ->
            {id, Id} = lists:keyfind(id, 1, DevInfo),
            {upstream, DevUpstream} = lists:keyfind(upstream, 1, DevInfo),
            %store_upstream_in_ets(Id, DevUpstream),
            {timeout, Timeout} = lists:keyfind(timeout, 1, DevInfo),
            {refresh, Refresh} = lists:keyfind(refresh, 1, DevInfo),
            {Id,
                {umeteo_dev_upstream, start_link,
                    [Id, DevUpstream, Timeout, Refresh]},
                permanent,
                5000,
                worker,
                [umeteo_dev_mon]}
        end, DevList)
    }}.
