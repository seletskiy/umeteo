-behaviour(supervisor).

-export([
    start_link/0,
    init/1
]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init(_Args) ->
    {ok, {{one_for_one, 5, 60}, [
        {umeteo_config,
            {umeteo_config, start_link, []},
            permanent,
            brutal_kill,
            worker,
            [umeteo_config]},
        {umeteo_mon_sup,
            {umeteo_mon_sup, start_link, []},
            permanent,
            brutal_kill,
            worker,
            [umeteo_mon_sup]}
    ]}}.
