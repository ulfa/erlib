%% Copyright (c) 2013 Ulf Angermann
%% See MIT-LICENSE for licensing information.

%%% -------------------------------------------------------------------
%%% Author  : Ulf Angermann uaforum1@googlemail.com
%%% Description :
%%%
%%% Created : 

-module(date).
-compile(export_all).

-define(WOCHENTAG(Int), lists:nth(Int, ["Montag","Dienstag", "Mittwoch", "Donnerstag", "Freitag", "Samstag", "Sonntag"])).

get_date_seconds() ->
    calendar:datetime_to_gregorian_seconds(calendar:local_time()).

is_date_in_range(Akt_date, Date) ->	
	calendar:datetime_to_gregorian_seconds(Akt_date) - calendar:datetime_to_gregorian_seconds(Date) < 0.

is_date_in_range(Date) ->
	is_date_in_range(calendar:universal_time(), Date).
		
day_of_week(Int) ->
	?WOCHENTAG(Int).

create_date_from_string([]) ->
	{erlang:date(), {00,00,00}};
create_date_from_string(Date) ->
	[Y,M,D] = string:tokens(Date, "-"),
	{{erlang:list_to_integer(Y), erlang:list_to_integer(M) ,erlang:list_to_integer(D)}, {00,00,00}}.

create_simple_date_from_string([]) ->
	erlang:date();	
create_simple_date_from_string(Date) ->
	[Y,M,D] = string:tokens(Date, "-"),
	{erlang:list_to_integer(Y), erlang:list_to_integer(M) ,erlang:list_to_integer(D)}.

create_from_date(Date) ->
	D = create_simple_date_from_string(Date),
	{D, {0,0,0}}.

create_to_date(Date) ->
	D = create_simple_date_from_string(Date),
	{D, {23,59,59}}.
	
create_date_string(Date) ->
	{{Year,Month,Day},{Hour,Min,Seconds}} = Date,
	Args = [Year, Month, Day],
	lists:flatten(io_lib:format("~B-~2..0B-~2..0B", Args)).

create_date_string_from_date(Date) ->
	create_date_string({Date, {0,0,0}}).

create_date_german_string(Date) ->
	{{Year,Month,Day},{Hour,Min,Seconds}} = Date,
	Args = [Day,Month,Year],
	lists:flatten(io_lib:format("~2..0B.~2..0B.~B", Args)).
	
create_actual_date() ->
	calendar:local_time().

week_of_year() ->
	{_Year, Week} = calendar:iso_week_number(),
	Week.

complete_actual_date() ->	
	complete_date(erlang:date()).
	
complete_date(Date) ->
	{_Year, Week} = calendar:iso_week_number(Date),	
	{Date, Week, calendar:day_of_the_week(Date)}.
	
construct_date({Y, M, D}) ->
	lists:concat([Y ,"-" ,M ,"-", D]).
	
get_formated_date(Date) ->
	{{Year,Month,Day},{Hour,Min,Seconds}} = Date,
	Args = [Year, Month, Day, Hour, Min, Seconds],
	A = io_lib:format("~B-~2.10.0B-~2.10.0B ~2.10.0B:~2.10.0B:~2.10.0B", Args),
	lists:flatten(A).

get_formated_date_for_now(Now) ->
	get_formated_date(calendar:now_to_local_time(Now)).

get_first_day({Y, M, _D}) ->
	FromDate = date_lib:create_date_string({{Y, M, 1}, {0,0,0}}).
	
get_last_day({Y, M, D}) ->	
	Last_day = calendar:last_day_of_the_month(Y, M),
	ToDate = date_lib:create_date_string({{Y, M, Last_day}, {0,0,0}}).
	
get_days_of_month(Year, Month) ->
	list_of_integer_to_string(lists:seq(1, calendar:last_day_of_the_month(list_to_integer(Year), list_to_integer(Month)))).
	
list_of_integer_to_string(List_of_integer) ->
	string:join([integer_to_list(S) || S <- List_of_integer],",").	
					
get_timestamp() ->
	erlang:list_to_binary(erlang:integer_to_list(calendar:datetime_to_gregorian_seconds(calendar:local_time()))).

timestamp_to_date(Time) when is_binary(Time) ->
	date:get_formated_date(calendar:gregorian_seconds_to_datetime(erlang:list_to_integer(erlang:binary_to_list(Time)))).

seconds_to_date(Seconds) when is_list(Seconds) ->
	date:get_formated_date(calendar:gregorian_seconds_to_datetime(erlang:list_to_integer(Seconds))).

format_uptime(UpTime) ->
	{D, {H, M, S}} = calendar:seconds_to_daystime(UpTime div 1000),
	lists:flatten(io_lib:format("~p days, ~p hours, ~p minutes and ~pseconds", [D,H,M,S])).

-include_lib("eunit/include/eunit.hrl").
-ifdef(TEST).

is_date_in_range_test() ->
	?assertEqual(true, is_date_in_range({{2012,10,01}, {0,0,0}}, {{2012,11,01},{0,0,0}})),
	?assertEqual(false, is_date_in_range({{2012,10,01}, {0,0,0}}, {{2012,09,01},{0,0,0}})).

day_of_week_test() ->
	?assertEqual("Montag", day_of_week(1)),
	?assertEqual("Sonntag", day_of_week(7)).

create_date_from_string_test() ->
	?assertEqual({{2012,10,30}, {00,00,00}}, create_date_from_string("2012-10-30")).
	
create_date_string_test() ->
	?assertEqual("2012-10-20", create_date_string({{2012,10,20}, {0,0,0}})).
	
create_date_german_string_test() ->
		?assertEqual("20.10.2012", create_date_german_string({{2012,10,20}, {0,0,0}})).
get_formated_date_test() ->
	?assertEqual("2013-05-23 11:39:02", get_formated_date({{2013,5,23},{11,39,2}})).

get_formated_date_for_now_test() ->
	?assertEqual("2013-05-23 11:44:05", get_formated_date_for_now({1369,302245,468365})).

-endif.
