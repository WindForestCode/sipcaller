
-module(user_handler).

-behaviour(cowboy_handler).

-export([init/2]).

init(Req0, State) ->
    Method = cowboy_req:method(Req0),
    Id = cowboy_req:binding(userid, Req0),
    handle_request(Method, Id, Req0, State).

handle_request(<<"GET">>, Id, Req0, State) ->
    io:format("Get ID -->~p~n", [Id]),
    case sip_client_db:get_contact(binary_to_integer(Id)) of
        user_not_found ->
            Req = cowboy_req:reply(404,
                #{<<"content-type">> => <<"application/json">>},
                jsx:encode(#{<<"error">> => <<"UserId not found">>}),
                Req0
            );
        Contact ->
            sip_server:call_user(Contact),
            Req = cowboy_req:reply(200,
                #{<<"content-type">> => <<"application/json">>},
                Req0
                )
    end,
    {ok, Req, State}.    
