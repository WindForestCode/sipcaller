%%%-------------------------------------------------------------------
%% @doc sip_pingpong public API
%% @end
%%%-------------------------------------------------------------------

-module(sip_pingpong_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    create_tables(),
    Res = sip_pingpong_sup:start_link(),
    sip_pingpong_sup:dump(),
    Res.

stop(_State) ->
    close_tables(),
    ok.

create_tables() ->
    ets:new(usersRam, [named_table,
        set,
        {keypos, 1},
        public]),
    io:format("ETS storage created~n").

close_tables() ->
    ets:delete(usersRam).
