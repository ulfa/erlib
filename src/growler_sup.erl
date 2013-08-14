%% Copyright (c) 2013 Ulf Angermann
%% See MIT-LICENSE for licensing information.

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