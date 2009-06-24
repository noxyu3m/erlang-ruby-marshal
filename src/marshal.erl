-module(marshal).

-export([parse_file/1, parse/1, encode/1]).
-export([test/0]).

-include("marshal.hrl").

parse_file(Filename) ->
    case file:read_file(Filename) of
        {ok, D} -> parse(D);
        Any -> Any
    end.

%% parse/1

parse(<<Major:8, Minor:8, D/binary>>) when Major =:= ?MARSHAL_MAJOR, Minor =:= ?MARSHAL_MINOR ->
    parse(D);
parse(D) -> parse(D, []).

%% parse/2

parse(<<>>, Acc) ->
    lists:reverse(Acc);
parse(<<T:8, D/binary>>, Acc) ->
    {Element, D2} = parse_element(T, D),
    parse(D2, [Element | Acc]).
    
encode(Term) ->
  Binary = encode_element(Term),
  <<?MARSHAL_MAJOR:8, ?MARSHAL_MINOR:8, Binary/binary>>.

%% parse_element/2

parse_element(?TYPE_NIL, <<D/binary>>) -> {nil, D};
parse_element(?TYPE_TRUE, <<D/binary>>) -> {true, D};
parse_element(?TYPE_FALSE, <<D/binary>>) -> {false, D};

parse_element(?TYPE_FIXNUM, <<S:8, D/binary>>) -> parse_fixnum(S, D);
parse_element(?TYPE_FLOAT, <<S:8, D/binary>>) -> parse_float(S, D);

parse_element(?TYPE_STRING, <<S:8, D/binary>>) -> parse_string(S, D);
parse_element(?TYPE_REGEXP, <<S:8, D/binary>>) -> parse_regexp(S, D);

parse_element(?TYPE_ARRAY, <<S:8, D/binary>>) -> parse_array(S, D);
parse_element(?TYPE_HASH, <<S:8, D/binary>>) -> parse_hash(S, D);

parse_element(?TYPE_SYMBOL, <<S:8, D/binary>>) -> parse_symbol(S, D);
parse_element(?TYPE_UCLASS, <<D/binary>>) -> parse_uclass(D);

parse_element(_T, <<T:8, D/binary>>) -> parse_element(T, D).

%% encode_element/1

encode_element(nil) -> <<?TYPE_NIL:8>>;
encode_element(null) -> encode_element(nil);
encode_element(undefined) -> encode_element(nil);
encode_element(true) -> <<?TYPE_TRUE:8>>;
encode_element(false) -> <<?TYPE_FALSE:8>>;
encode_element(A) when is_integer(A), A =< 2147483647, A >= -2147483648 -> encode_fixnum(A);
encode_element(A) when is_integer(A), A > 2147483647 -> encode_bignum(A);
encode_element(A) when is_integer(A), A < 2147483648 -> encode_bignum(A);
encode_element(A) when is_float(A) -> encode_float(A);
encode_element({string, A}) -> encode_string(A);
encode_element({regexp, A}) -> encode_regexp(A);
encode_element({array, A}) -> encode_array(A).

%% TODO
%encode_element({hash, A}) -> encode_hash(A);
%encode_element({symbol, A}) -> encode_symbol(A).

%% Base types - parse

parse_fixnum(S, D) ->
    unpack(S, D).

parse_float(S, D) ->
    {Float, D2} = parse_string(S, D),
    {list_to_float(Float), D2}.

parse_string(S, D) ->
    {Size, D2} = unpack(S, D),
    read_bytes(D2, Size).

parse_regexp(S, D) ->
    {RegExp, D2} = parse_string(S, D),
    <<_:8, D3/binary>> = D2,
    {{regexp, RegExp}, D3}.
    
%% Base types - encode

encode_fixnum(A) ->
  Binary = pack(A),
  <<?TYPE_FIXNUM:8, Binary/binary>>.
  
encode_float(A) ->
  Binary = list_to_binary(float_to_list(A)),
  <<?TYPE_FLOAT:8, Binary/binary>>.
  
encode_bignum(A) ->
  Sign = case A > 0 of
            true -> $+;
            _ -> $-
         end,
  Nbits = nbits_unsigned(A),
  Size = pack(trunc(Nbits / 16)),
  <<?TYPE_BIGNUM:8, Sign:8, Size/binary, A:Nbits/little-unsigned>>.
  
encode_string(A) ->
  Binary = unicode:characters_to_binary(A),
  Size = pack(lists:flatlength(A)),
  <<?TYPE_STRING:8, Size/binary, Binary/binary>>.

encode_regexp(A) ->
  Binary = unicode:characters_to_binary(A),
  Size = pack(lists:flatlength(A)),
  <<?TYPE_REGEXP:8, Size/binary, Binary/binary>>.

encode_array(List) ->
  Size = pack(lists:flatlength(List)),
  Binary = encode_array(List, <<>>),
  <<?TYPE_ARRAY:8, Size/binary, Binary/binary>>. 

encode_array([], Acc) ->
  Acc;
encode_array(List, Acc) ->
  [Head | Tail] = List,
  Binary = encode_element(Head),
  encode_array(Tail, <<Acc/binary, Binary/binary>>).

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

parse_symbol(S, D) ->
    {Size, D2} = unpack(S, D),
    {Symbol, D3} = read_bytes(D2, Size),
    {list_to_atom(Symbol), D3}.

parse_uclass(<<T:8, D/binary>>) ->
    {_ClassName, D2} = parse_element(T, D),
    <<T2:8, _:8, S:8, D3/binary>> = D2,
    D4 = list_to_binary([S] ++ binary_to_list(D3)),
    parse_element(T2, D4).

%% Helpers

pack(0) ->
  <<0:8>>;
pack(N) when N >= 1, N =< 122 ->
  N2 = N + 5,
  <<N2:8>>;
pack(N) when N =< -1, N >= -122 ->
  N2 = N - 5,
  <<N2:8>>;
pack(N) when N >= 123, N =< 2147483647 ->
  <<4:8, N:32/little-unsigned>>;
pack(N) when N =< -123, N >= -2147483648 ->
  <<-4:8, N:32/little-unsigned>>.
  
unpack(N, D) when N =:= 0 ->
    {N, D};
unpack(N, D) when N >= 6, N =< 127 ->
    {N - 5, D};
unpack(N, D) when N >= 1, N =< 4 ->
    {N2, D2} = read_bytes(D, N),
    N3 = read_integer(list_to_binary(N2 ++ [0, 0, 0])),
    {N3, D2};
unpack(N, D) when N =< -6, N >= -128 ->
    {N + 5, D}.

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

nbits_unsigned(XS) -> % Necessary bit size for an integer value.
    Min = trunc(math:log(XS) / math:log(2)) + 1,
    case Min rem 16 of
      0 -> Min;
      _ -> Min - (Min rem 16) + 16
    end.

%% Tests

test() ->
    [{fixnum_test, fixnum_test()},
     {float_test, float_test()},
     {string_test, string_test()},
     {regexp_test, regexp_test()},
     {array_test, array_test()},
     {hash_test, hash_test()}].

fixnum_test() ->
    need_implementation.

float_test() ->
    [3.141592653589793] =:= parse_file("tests/float_test.bin").

string_test() ->
    ["Hello, world !!!"] =:= parse_file("tests/string_test.bin").

regexp_test() ->
    need_implementation.

array_test() ->
    [[2, 4, 8, 16, 32]] =:= parse_file("tests/array_test.bin").

hash_test() ->
    need_implementation.
