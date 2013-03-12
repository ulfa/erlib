%% Copyright 2010 Ulf Angermann
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%% 
%%     http://www.apache.org/licenses/LICENSE-2.0
%% 
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.

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