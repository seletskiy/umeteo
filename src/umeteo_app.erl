%% @author Stanislav Seletskiy <s.seletskiy@gmail.com>
%% @doc umeteo app.
-module(umeteo_app).
-created('Date: 02/10/2013').
-created_by('Stanislav Seletskiy <s.seletskiy@gmail.com>').
-behaviour(application).

-export([start/2, stop/1]).

start(_Type, _Args) ->
    application:start(sl),
    application:start(umb),
    application:start(ranch),
    application:start(crypto),
    application:start(cowboy),
    sl:install(log),
    sl:info("init"),
    {ok, ConfigFile} = application:get_env(config),
    sl:info("using config file <~s>", [ConfigFile]),
    {ok, Config} = file:consult(ConfigFile),
    umeteo_sup:start_link(Config).

stop(_State) ->
    cowboy:stop_listener(umeteo_rest_api),
    sl:info("stop"),
    ok.
