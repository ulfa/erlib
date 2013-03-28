-module(converter).
-export([proplists_to_jsx_input/1]).

proplists_to_jsx_input(List) ->
	[{transform(X),transform(Y)} || {X,Y} <- List].
	
transform([]) ->
	[];
transform(true) ->
	true;
transform(false) ->
	false;	
transform(Value) when is_pid(Value) ->
	list_to_binary(pid_to_list(Value));
transform(Value) when is_atom(Value) ->
	erlang:atom_to_binary(Value, utf8);
transform([H|T]) when is_list(H) ->
	[transform(X)||X<-[H|T]];
transform([H|T]) when is_tuple(H) ->
	proplists_to_jsx_input([H|T]);
transform(Value) when is_tuple(Value) ->
	[transform(X)||X<-tuple_to_list(Value)];
transform(Value) when is_list(Value) ->
	case is_string(Value) of 
		true -> list_to_binary(Value);
		false -> [transform(X)||X<-Value]
	end;
transform(Value) ->
	Value.

is_string([]) -> true;
is_string([X|T]) -> is_integer(X) andalso X>=0 andalso is_string(T);
is_string(_) -> false.

-include_lib("eunit/include/eunit.hrl").
-ifdef(TEST).

proplists_to_jsx_input_test() ->
	A = [{a, true}, {b, "test"}, {c, 1}],
	B = [{<<"a">>, true}, {<<"b">>, <<"test">>}, {<<"c">>, 1}],
	?assertEqual(B, proplists_to_jsx_input(A)).
	
is_string_test() ->
	?assertEqual(true, is_string("test")).

transform_test() ->
	?assertEqual(<<"test">>, transform(test)),
	?assertEqual(true, transform(true)),
	?assertEqual(false, transform(false)),
	?assertEqual(1, transform(1)),
	?assertEqual(1.0, transform(1.0)),
	?assertEqual(<<"test">>, transform("test")),
	?assertEqual([{<<"a">>,<<"b">>}, {<<"c">>,<<"d">>}], transform([{a,b}, {c,d}])),
	?assertEqual([], transform([])),
	?assertEqual(<<"<0.0.1>">>, transform(erlang:list_to_pid("<0.0.1>"))),
	?assertEqual([<<"n">>, <<"s">>, <<"<8644.48.0>">>], transform([n,s,"<8644.48.0>"])),
	?assertEqual([{<<"a">>, [<<"n">>, <<"s">>, <<"<8644.48.0>">>]}], transform([{"a",[n,s,"<8644.48.0>"]}])),
	?assertEqual([<<"<0.0.0>">>], transform(["<0.0.0>"])),
	?assertEqual([<<"test">>, <<"test2">>], transform(["test", "test2"])),
	?assertEqual([<<"a">>,<<"b">>, <<"c">>], transform({a,b,c})),
	?assertEqual([{<<"a">>,<<"b">>}, {<<"c">>,<<"d">>}], proplists_to_jsx_input([{a,b}, {c,d}])).

-endif.
