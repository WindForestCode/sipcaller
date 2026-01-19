%%%-------------------------------------------------------------------
%% @doc sip_pingpong public API
%% @end
%%%-------------------------------------------------------------------

-module(sip_pingpong_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    create_tables(),

    start_cowboy(),

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

start_cowboy() ->
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/api/call/:userid", user_handler, []}
            ]}
        ]),

    {ok, _} = cowboy:start_clear(
        my_http_listener,
        [{port, 8080}],
        #{env => #{dispatch => Dispatch}}
    ),
    io:format("COWBOY STARTED~n").