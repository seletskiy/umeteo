%% @author Stanislav Seletskiy <s.seletskiy@gmail.com>
%% @doc Umeteo Main Supervisor.
-module(umeteo_sup).
-created('Date: 10/10/2013').
-created_by('Stanislav Seletskiy <s.seletskiy@gmail.com>').
-behaviour(supervisor).

-export([
    start_link/1,
    init/1
]).

start_link(Config) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, [Config]).

init([Config]) ->
    {port, ListenPort} = lists:keyfind(port, 1, Config),
    {nb_accept, NbAccept} = lists:keyfind(nb_accept, 1, Config),
    {devices_conf, DevicesConfig} = lists:keyfind(devices_conf, 1, Config),
    sl:info("starting cowboy listeners (~B) at port ~B", [NbAccept, ListenPort]),
    Routes = cowboy_router:compile([
        {'_', [
            {"/v1/devices", umeteo_http_devices_handler, []},
            {"/v1/devices/:dev_id/values", umeteo_http_values_handler, []},
            {"/v1/devices/:dev_id/values/subscribe", umeteo_http_values_subscribe, []}
        ]}]),
    pg2:create(umeteo_listeners),
    {ok, {{one_for_one, 5, 60}, [
        {umeteo_dev_sup,
            {umeteo_dev_sup, start_link, [DevicesConfig]},
            permanent,
            5000,
            supervisor,
            [umeteo_dev_sup]},
        ranch:child_spec(umeteo_rest_api, NbAccept,
            ranch_tcp, [{port, ListenPort}],
            cowboy_protocol, [{env, [{dispatch, Routes}]}])
    ]}}.
