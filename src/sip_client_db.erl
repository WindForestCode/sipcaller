
-module(sip_client_db).

-behaviour(gen_server).

-include("include/uri.hrl").

-export([start_link/0, save_contact/1, get_contact/1, create_tables/0]).
-export([init/1, handle_call/3, handle_cast/2, terminate/2]).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% Callback functions
init([]) ->
    create_tables(),
    {ok, null}.

handle_call(_Request, _From, LoopData) ->
    {reply, null, LoopData}.

handle_cast(_Msg, LoopData) ->
    {noreply, LoopData}.

terminate(_Reason, _LoopData) ->
    close_tables().

%% API
save_contact(Contact) ->
    io:format("sip database - trying to save Contact - ~p~n", [Contact]),
    [Tuple | _ ] = Contact, 
    UserId = element(3, Tuple),
    ets:insert(usersRam, {UserId, Contact}).

get_contact(UserId) ->
    BinaryId = integer_to_binary(UserId),
    case ets:lookup(usersRam, BinaryId) of
        [{BinaryId, Contact}] -> Contact;
        [] -> user_not_found
    end.

create_tables() ->
    ets:new(usersRam, [named_table,
        set,
        {keypos, 1},
        public]),
    io:format("ETS storage created~n").

close_tables() ->
    ets:delete(usersRam).