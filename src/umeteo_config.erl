-behaviour(gen_server).

-export([
    start_link/0
]).

-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

-record (state, {
    path :: string()
}).

start_link(ConfigPath) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [ConfigPath], []).

load_config(File) ->
    Result = case file:consult(State#state.path) of
        {ok, Terms} ->
            apply_config(Terms);
        {error, Error} ->
            sl:error("error parsing config: ~p", [Error]),
            {error, Error}
    end.

apply_config(Config) ->
    ok.

%% @hidden
init([ConfigPath]) ->
    {ok, #state{path = ConfigPath}}.

%% @hidden
handle_call(load, _From, State) ->
    {reply, load_config(State#state.path), State};
handle_call(_Message, _From, State) ->
    {noreply, State}.

%% @hidden
handle_cast(_Message, State) ->
    {noreply, State}.

%% @hidden
handle_info(_Info, State) ->
    {noreply, State}.

%% @hidden
terminate(_Reason, _State) ->
    ok.

%% @hidden
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
