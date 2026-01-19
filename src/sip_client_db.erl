
-module(sip_client_db).

-include("include/uri.hrl").

-export([save_contact/1, get_contact/1]).

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