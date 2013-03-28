-module(converter).
-export([proplists_to_jsx_input/1]).

proplists_to_jsx_input(List) ->
	[{transform(X),transform(Y)} || {X,Y} <- List].
	
transform(true) ->
	true;
transform(false) ->
	false;	
transform(Value) when is_atom(Value) ->
	erlang:atom_to_binary(Value, utf8);
transform([]) ->
	[];

transform([H|T]) when is_list(H) ->
	[transform(X)||X<-[H|T]];
transform(Value) when is_list(Value) ->
	case is_tuple_list(Value) of 
		true -> proplists_to_jsx_input(Value);
		false -> erlang:list_to_binary(Value)
	end;
transform(Value) when is_tuple(Value) ->
	[transform(X)||X<-tuple_to_list(Value)];
transform(Value) ->
	Value.
is_tuple_list([]) ->	
	false;
is_tuple_list([H|_T]) ->
	is_tuple(H). 

-include_lib("eunit/include/eunit.hrl").
-ifdef(TEST).

proplists_to_jsx_input_test() ->
	A = [{a, true}, {b, "test"}, {c, 1}],
	B = [{<<"a">>, true}, {<<"b">>, <<"test">>}, {<<"c">>, 1}],
	?assertEqual(B, proplists_to_jsx_input(A)).
	
transform_test() ->
	?assertEqual(<<"test">>, transform(test)),
	?assertEqual(true, transform(true)),
	?assertEqual(false, transform(false)),
	?assertEqual(1, transform(1)),
	?assertEqual(1.0, transform(1.0)),
	?assertEqual(<<"test">>, transform("test")),
	?assertEqual([{<<"a">>,<<"b">>}, {<<"c">>,<<"d">>}], transform([{a,b}, {c,d}])),
	?assertEqual([], transform([])),
	?assertEqual([<<"<0.0.0>">>], transform(["<0.0.0>"])),
	?assertEqual([<<"test">>, <<"test2">>], transform(["test", "test2"])),
	?assertEqual([<<"a">>,<<"b">>, <<"c">>], transform({a,b,c})),
	?assertEqual([{<<"a">>,<<"b">>}, {<<"c">>,<<"d">>}], proplists_to_jsx_input([{a,b}, {c,d}])).
-endif.
