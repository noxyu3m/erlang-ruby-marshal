-module(rcookie).
-export([start/0, stop/0, parse/1, test/0]).

-define(SECRET, "6b2251cd99b7f73aa72342a5573dea4942d76203c27c1884ee7a995e3a090bee36535935a9d88c566c27eba2be63c579055b5b303964626b255c55968fb70041").

start() ->
    application:start(crypto).

stop() ->
    application:stop(crypto).

parse(Cookie) ->
    [Data, Digest] = string:tokens(Cookie, "--"),
    case verify(Data, Digest) of
        false -> {error, verify_failed};
        true -> {ok, marshal:parse(base64:decode(Data))}
    end.

verify(Data, Digest) ->
    DataDigest = generate_digest(Data),
    DataDigest =:= Digest.

generate_digest(Data) ->
    lists:flatten(list_to_hex(binary_to_list(crypto:sha_mac(?SECRET, Data)))).

%% http://sacharya.com/md5-in-erlang/

list_to_hex(L) ->
    lists:map(fun(X) -> int_to_hex(X) end, L).

int_to_hex(N) when N < 256 ->
    [hex(N div 16), hex(N rem 16)].

hex(N) when N < 10 ->
    $0 + N;
hex(N) when N >= 10, N < 16 ->
    $a + (N - 10).

%% Tests

test() ->
    start(),
    {cookie_test, cookie_test()}.

cookie_test() ->
    {ok, Cookie} = file:read_file("tests/cookie_test.txt"),
    {ok, [[{user_id,1},
           {'_csrf_token',"6BiVBeXR9BszDGYJ3el/WLW+MQpOyGOInH3CZLiPm5A="},
           {"flash",[{'@used',[]}]},
           {session_id,"93dd730dac6795ffabdb65a2e2447103"}]]} =:= parse(binary_to_list(Cookie)).
