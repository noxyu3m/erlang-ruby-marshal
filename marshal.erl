-module(marshal).
-export([parse_file/1, parse/1]).

-define(MARSHAL_MAJOR, 4).
-define(MARSHAL_MINOR, 8).

-define(NIL, 48).
-define(TRUE, 84).
-define(FALSE, 70).

-define(FIXNUM, 105).

-define(STRING, 34).
-define(ARRAY, 91).
-define(HASH, 123).

-define(SYMBOL, 58).

parse_file(Filename) ->
    case file:read_file(Filename) of
        {ok, D} -> parse(D);
        Any -> Any
    end.

%% parse/1

parse(<<Major:8, Minor:8, D/binary>>) when Major =:= ?MARSHAL_MAJOR, Minor =:= ?MARSHAL_MINOR ->
    parse(D);
parse(D) ->
    parse(D, []).

%% parse/2

parse(<<>>, Acc) ->
    lists:reverse(Acc);
parse(<<T:8, D/binary>>, Acc) ->
    {Element, D2} = parse_element(T, D),
    parse(D2, [Element | Acc]).

parse_element(?NIL, <<D/binary>>) ->
    {nil, D};
parse_element(?TRUE, <<D/binary>>) ->
    {true, D};
parse_element(?FALSE, <<D/binary>>) ->
    {false, D};
parse_element(?FIXNUM, <<S:8, D/binary>>) ->
    parse_fixnum(S, D);
parse_element(?STRING, <<S:8, D/binary>>) ->
    parse_string(S, D);
parse_element(?ARRAY, <<S:8, D/binary>>) ->
    parse_array(S, D);
parse_element(?HASH, <<S:8, D/binary>>) ->
    parse_hash(S, D);
parse_element(?SYMBOL, <<S:8, D/binary>>) ->
    parse_symbol(S, D).

parse_fixnum(S, D) ->
    unpack(S, D).

parse_string(S, D) ->
    {Size, D2} = unpack(S, D),
    read_bytes(D2, Size).

parse_symbol(S, D) ->
    {Size, D2} = unpack(S, D),
    {Symbol, D3} = read_bytes(D2, Size),
    {list_to_atom(Symbol), D3}.

%% Array

parse_array(S, D) ->
    {Size, D2} = unpack(S, D),
    parse_array(D2, Size, []).

parse_array(D, 0, Acc) ->
    {lists:reverse(Acc), D};
parse_array(D, Size, Acc) ->
    <<T:8, D2/binary>> = D,
    {Element, D3} = parse_element(T, D2),
    parse_array(D3, Size - 1, [Element | Acc]).

%% Hash

parse_hash(S, D) ->
    {Size, D2} = unpack(S, D),
    parse_hash(D2, Size, []).

parse_hash(D, 0, Acc) ->
    {lists:reverse(Acc), D};
parse_hash(D, Size, Acc) ->
    {{Key, Value}, D2} = parse_hash_element(D),
    parse_hash(D2, Size - 1, [{Key, Value} | Acc]).

parse_hash_element(<<T:8, D/binary>>) ->
    {Key, D2} = parse_element(T, D),
    <<T2:8, D3/binary>> = D2,
    {Value, D4} = parse_element(T2, D3),
    {{Key, Value}, D4}.

%% Helpers

unpack(N, D) when N =:= 0 ->
    {N, D};
unpack(N, D) when N >= 6, N =< 127 ->
    {N - 5, D};
unpack(N, D) when N >= 1, N =< 4 ->
    {N2, D2} = read_bytes(D, N),
    N3 = read_integer(list_to_binary(N2 ++ [0, 0, 0])),
    {N3, D2}.

read_integer(<<N:32/little-unsigned>>) ->
    N;
read_integer(<<N:32/little-unsigned, _>>) ->
    N.

read_bytes(Data, Count) ->
    read_bytes(Data, Count, []).

read_bytes(Data, 0, Acc) ->
    {Acc, Data};
read_bytes(<<Byte:8, Data/binary>>, Count, Acc) ->
    read_bytes(Data, Count - 1, Acc ++ [Byte]).
