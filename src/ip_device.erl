%% Copyright (c) 2013 Ulf Angermann
%% See MIT-LICENSE for licensing information.

%%% -------------------------------------------------------------------
%%% Author  : Ulf Angermann uaforum1@googlemail.com
%%% Description :
%%%
%%% Created : 
%%% -------------------------------------------------------------------
-module(ip_device).

-export([get_ip/0, ip_as_string/1]).

get_ip() ->
	get_active_ip().

get_active_ip() ->
	get_active_ip(get_iflist()).

get_active_ip(If_list) ->
	get_ip([A || A <- If_list, inet:ifget(A,[addr]) /= {ok,[{addr,{127,0,0,1}}]}, filter_networkcard(list_to_binary(A))]).

filter_networkcard(<<"vnic", _R/binary>>) ->
	false;
filter_networkcard(<<"vmnet", _R/binary>>) ->
	false;
filter_networkcard(_) ->
	true.

get_ip([]) ->
	get_loopback();

get_ip([If]) ->
	case inet:ifget(If, [addr]) of
		{ok, []} -> get_loopback();
		{_, [{_, Ip}]} -> Ip
	end.

get_loopback() ->
	get_loopback(get_iflist()).

get_loopback(If_list) ->
	get_ip([A || A <- If_list, inet:ifget(A,[addr]) == {ok,[{addr,{127,0,0,1}}]}]).

get_iflist() ->
	{ok, IfList} = inet:getiflist(),
	IfList.
	
ip_as_string(Ip) ->
	inet_parse:ntoa(Ip).