%% @author Stanislav Seletskiy <s.seletskiy@gmail.com>
%% @doc Umeteo Start Script.
-module(umeteo).
-created('Date: 14/10/2013').
-created_by('Stanislav Seletskiy <s.seletskiy@gmail.com>').

-export([
    start/0
]).

start() ->
    application:start(umeteo).
