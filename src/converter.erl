-module(converter).
-export([proplists_to_jsx_input/1]).

proplists_to_jsx_input(List) ->
	[{transform(X),transform(Y)}||{X,Y}<-List].
	
transform(true) ->
	true;
transform(false) ->
	false;	
transform(Value) when is_atom(Value) ->
	erlang:atom_to_binary(Value, utf8);
transform(Value) when is_list(Value)->
	erlang:list_to_binary(Value);
transform(Value) ->
	Value.
	
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
	?assertEqual(<<"test">>, transform("test")).
-endif.
