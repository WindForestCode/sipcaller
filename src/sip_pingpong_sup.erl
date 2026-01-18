%%%-------------------------------------------------------------------
%% @doc sip_pingpong top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(sip_pingpong_sup).

-behaviour(supervisor).

-export([start_link/0, init/1]).

-export([dump/0]).
-export([print_tree/3]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    ChildSpecs = [
        %% SIP-сервер для приема звонков (ping)
        nksip:get_sup_spec(sip_server, #{
            sip_local_host => "localhost",
            plugins => [nksip_registrar],
            sip_listen => "sip:all:5060"
        }),
        %% SIP-клиент для обратного звонка (pong)
        nksip:get_sup_spec(sip_client, #{
            sip_local_host => "localhost",
            sip_from => "sip:sip_client@127.0.0.1",
            plugins => [nksip_uac_auto_auth],
            sip_listen => "sip:127.0.0.1:5075"
        }),
        %% Модуль для БД, как gen_server
        #{
            id => sip_client_db,
            start => {sip_client_db, start_link, []},
            restart => permanent,
            type => worker
          }
    ],
    io:format("sip_pingpong_sup: ChildSpecs ~p~n", [ChildSpecs]),
    SupFlags = #{
        strategy => one_for_one,  %% Перезапускается только отказавший процесс, дочерние процессы независимы
        intensity => 10,          %% Ограничение в 10 перезапусков за 60 секунд
        period => 60
    },
    {ok, {SupFlags, ChildSpecs}}.

% sip_pingpong_sup:dump().
dump() ->
    io:format("++++ Supervisor tree: sip_pingpong_sup ~p~n", [whereis(sip_pingpong_sup)]),
    print_tree(whereis(sip_pingpong_sup), sip_pingpong_sup, 0),
    io:format("~n+++ ~p~n", [sys:get_state(whereis(sip_client))]),
    io:format("~n+++ ~p~n", [sys:get_status(whereis(sip_client))]).

print_tree(Proc, ProcId, Level) ->
    RegName = erlang:process_info(Proc, registered_name),
    case catch supervisor:which_children(Proc) of
        {'EXIT', _} ->
            indent(Level),
            io:format("+-- ~p ~p <worker> ~p~n", [Proc, ProcId, RegName]);
        Children when is_list(Children) ->
            indent(Level),
            io:format("+-- ~p ~p <supervisor> ~p~n", [Proc, ProcId, RegName]),
            lists:foreach(
                fun({Id, Child, Type, Modules}) ->
                    print_child(Id, Child, Type, Modules, Level + 1)
                end,
                Children
            )
    end.

print_child(Id, Child, supervisor, _Modules, Level) ->
    print_tree(Child, Id, Level);

print_child(Id, Child, worker, Modules, Level) ->
    indent(Level),
    RegName = erlang:process_info(Child, registered_name),
    io:format("+-- ~p ~p (worker, modules: ~p) ~p~n", [Child, Id, Modules, RegName]).

indent(Level) ->
    lists:foreach(
        fun(_) -> io:format("|    ") end,
        lists:seq(1, Level)
    ),
    io:format("").
