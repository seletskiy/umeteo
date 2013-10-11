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
    sl:info("reading devices conf from <~s>", [Config]),
    {ok, DevList} = file:consult(Config),
    {ok, {{one_for_one, 5, 60},
        lists:map(fun(DevInfo) ->
            {id, Id} = lists:keyfind(id, 1, DevInfo),
            {host, DevHost} = lists:keyfind(host, 1, DevInfo),
            {port, DevPort} = lists:keyfind(port, 1, DevInfo),
            {timeout, Timeout} = lists:keyfind(timeout, 1, DevInfo),
            {refresh, Refresh} = lists:keyfind(refresh, 1, DevInfo),
            {Id,
                {umeteo_dev_mon, start_link,
                    [Id, DevHost, DevPort, Timeout, Refresh]},
                permanent,
                5000,
                worker,
                [umeteo_dev_mon]}
        end, DevList)
    }}.
