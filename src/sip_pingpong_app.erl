%%%-------------------------------------------------------------------
%% @doc sip_pingpong public API
%% @end
%%%-------------------------------------------------------------------

-module(sip_pingpong_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    Res = sip_pingpong_sup:start_link(),
    sip_pingpong_sup:dump(),
    Res.

stop(_State) ->
    ok.
