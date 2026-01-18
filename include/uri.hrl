%% Заголовочный файл для uri, можно конретизировать названия полей
%% {uri,sip,<<"3003">>,<<>>,<<"127.0.0.1">>,5061,<<>>,
%%                              [],[],[],[],<<>>}
-record(uri, {
    type :: atom(),
    userId :: binary(),
    domain :: binary(),
    host :: binary(),
    port :: integer(),
    param1 :: binary(),
    param2 :: list(),
    param3 :: list(),
    param4 :: list(),
    param5 :: list(),
    param6 :: binary()
    }).