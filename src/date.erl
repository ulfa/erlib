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

get_date_seconds(Date_time) ->
    calendar:datetime_to_gregorian_seconds(Date_time).

get_start_datetime() ->
	get_formated_date(space, {erlang:date(), {00,00,00}}).

get_end_datetime() ->
	get_formated_date(space, {erlang:date(), {23,59,59}}).

is_date_in_range(Akt_date, Date) ->	
	calendar:datetime_to_gregorian_seconds(Akt_date) - calendar:datetime_to_gregorian_seconds(Date) < 0.

is_date_in_range(Date) ->
	is_date_in_range(calendar:universal_time(), Date).

is_time_in_range(Min_time, Max_time, Akt_time)->
	Akt_seconds = calendar:time_to_seconds(Akt_time),
	(calendar:time_to_seconds(Min_time) =< Akt_seconds) and (Akt_seconds =< calendar:time_to_seconds(Max_time)). 

day_of_week(Int) ->
	?WOCHENTAG(Int).

create_date_from_string([]) ->
	{erlang:date(), {00,00,00}};
create_date_from_string(Date) ->
	[Y,M,D] = string:tokens(Date, "-"),
	{{erlang:list_to_integer(Y), erlang:list_to_integer(M) ,erlang:list_to_integer(D)}, {00,00,00}}.

create_seconds_from_string(Date_time) ->
	[Date, Time] = string:tokens(Date_time, " "),
	[HH,MM,SS] = string:tokens(Time, ":"),
	[Y,M,D] = string:tokens(Date, "-"),
	DT = {{list_to_integer(Y), list_to_integer(M) ,list_to_integer(D)}, {list_to_integer(HH),list_to_integer(MM),list_to_integer(SS)}},
	get_date_seconds(DT).

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
	{{Year,Month,Day},{_Hour, _Min, _Seconds}} = Date,
	Args = [Year, Month, Day],
	lists:flatten(io_lib:format("~B-~2..0B-~2..0B", Args)).

create_date_string_from_date(Date) ->
	create_date_string({Date, {0,0,0}}).

create_date_german_string(Date) ->
	{{Year,Month,Day},{_Hour, _Min, _Seconds}} = Date,
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
	
get_formated_date(space, Date) ->
	{{Year,Month,Day},{Hour,Min,Seconds}} = Date,
	Args = [Year, Month, Day, Hour, Min, Seconds],
	A = io_lib:format("~B-~2.10.0B-~2.10.0B ~2.10.0B:~2.10.0B:~2.10.0B", Args),
	lists:flatten(A);

get_formated_date(minus, Date) ->
	get_formated_date("~B-~2.10.0B-~2.10.0B_~2.10.0B:~2.10.0B:~2.10.0B", Date);

get_formated_date(Format, Date) ->
	{{Year,Month,Day},{Hour,Min,Seconds}} = Date,
	Args = [Year, Month, Day, Hour, Min, Seconds],
	A = io_lib:format(Format, Args),
	lists:flatten(A).

get_formated_date_for_now(0) ->
	get_formated_date_for_now(now());

get_formated_date_for_now(Now) ->
	get_formated_date(space, calendar:now_to_local_time(Now)).

get_formated_date_for_seconds(Seconds) ->
	get_formated_date(minus, calendar:gregorian_seconds_to_datetime(Seconds)).

get_first_day({Y, M, _D}) ->
	date_lib:create_date_string({{Y, M, 1}, {0,0,0}}).
	
get_last_day({Y, M, _D}) ->	
	Last_day = calendar:last_day_of_the_month(Y, M),
	date_lib:create_date_string({{Y, M, Last_day}, {0,0,0}}).
	
get_days_of_month(Year, Month) ->
	list_of_integer_to_string(lists:seq(1, calendar:last_day_of_the_month(list_to_integer(Year), list_to_integer(Month)))).
	
list_of_integer_to_string(List_of_integer) ->
	string:join([integer_to_list(S) || S <- List_of_integer],",").	
					
get_timestamp() ->
	erlang:list_to_binary(erlang:integer_to_list(calendar:datetime_to_gregorian_seconds(calendar:local_time()))).

timestamp_to_date(Time) when is_binary(Time) ->
	date:get_formated_date(space, calendar:gregorian_seconds_to_datetime(erlang:list_to_integer(erlang:binary_to_list(Time)))).

seconds_to_date(Seconds) when is_list(Seconds) ->
	date:get_formated_date(space, calendar:gregorian_seconds_to_datetime(erlang:list_to_integer(Seconds)));

seconds_to_date(Seconds) when is_integer(Seconds) ->
	get_formated_date(space, calendar:gregorian_seconds_to_datetime(Seconds)).

format_uptime(UpTime) ->
	{D, {H, M, S}} = calendar:seconds_to_daystime(UpTime div 1000),
	lists:flatten(io_lib:format("~p days, ~p hours, ~p minutes and ~pseconds", [D,H,M,S])).

is_valid_date_time(Date_time) ->
	try 
		create_seconds_from_string(Date_time),
		true
	catch
		_:_Error -> false 
	end.

-include_lib("eunit/include/eunit.hrl").
-ifdef(TEST).

timestamp_to_date_test() ->
	?assertEqual("2015-05-17 07:54:48", timestamp_to_date(<<"63599068488">>)).

is_time_in_range_test() ->
	?assertEqual(true, is_time_in_range({11,00,00}, {17,00,00}, {16,00,00})),
	?assertEqual(true, is_time_in_range({11,00,00}, {16,00,00}, {16,00,00})),
	?assertEqual(false, is_time_in_range({11,00,00}, {16,00,00}, {16,00,01})),
	?assertEqual(true, is_time_in_range({11,00,00}, {17,00,00}, {11,00,00})),
	?assertEqual(false, is_time_in_range({11,00,00}, {17,00,00}, {10,59,59})).

is_valid_date_timetest() ->
	?assertEqual(true, is_valid_date_time("2014-06-12 00:00:00")),
	?assertEqual(false, is_valid_date_time("2014-06-12")).


is_date_in_range_test() ->
	?assertEqual(true, is_date_in_range({{2012,10,01}, {0,0,0}}, {{2012,11,01},{0,0,0}})),
	?assertEqual(false, is_date_in_range({{2012,10,01}, {0,0,0}}, {{2012,09,01},{0,0,0}})).

day_of_week_test() ->
	?assertEqual("Montag", day_of_week(1)),
	?assertEqual("Sonntag", day_of_week(7)).

create_date_from_string_test() ->
	?assertEqual({{2012,10,30}, {00,00,00}}, create_date_from_string("2012-10-30")).

create_seconds_from_string_test() ->
	?assertEqual(63569750400, create_seconds_from_string("2014-06-12 00:00:00")).

create_date_string_test() ->
	?assertEqual("2012-10-20", create_date_string({{2012,10,20}, {0,0,0}})).
	
create_date_german_string_test() ->
		?assertEqual("20.10.2012", create_date_german_string({{2012,10,20}, {0,0,0}})).
get_formated_date_test() ->
	?assertEqual("2013-05-23 11:39:02", get_formated_date(space, {{2013,5,23},{11,39,2}})).

get_formated_date_for_now_test() ->
	?assertEqual("2013-05-23 11:44:05", get_formated_date_for_now({1369,302245,468365})).

seconds_to_date_test() ->
	?assertEqual("2015-05-07 06:40:36", seconds_to_date(63598200036)).	

get_formated_date_for_seconds_test() ->	
	?assertEqual("2015-05-07_06:40:36", get_formated_date_for_seconds(63598200036)).

-endif.
