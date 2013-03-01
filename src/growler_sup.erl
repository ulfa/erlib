%% Copyright 2012 Ulf Angermann
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
%%% Created : 22.11.2012
%%% -------------------------------------------------------------------
-module(growler_sup).
-behaviour(supervisor).
%% --------------------------------------------------------------------
%% External exports
%% --------------------------------------------------------------------
-export([start_link/0]).
-export([init/1]).

%% ===================================================================
%% API functions
%% ===================================================================
start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================
init([]) ->
	ChildSpec = {growler, {growler, start_link, []}, temporary, brutal_kill, worker, [growler]},	
	RestartStrategy = {one_for_one, 1, 3600},
    {ok, {RestartStrategy, [ChildSpec]}}.