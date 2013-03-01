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
-module(growler).
-behaviour(gen_server).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
%% --------------------------------------------------------------------
%% External exports

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start_link/0]).
-export([start/0]).
-export([success/1, error/1, warning/1]).
-export([set_level/2]).

%% ====================================================================
%% External functions
%% ====================================================================
success(Message) ->
	gen_server:cast(?MODULE, {success, Message}).
error(Message) ->
	gen_server:cast(?MODULE, {error, Message}).	
warning(Message) ->
	gen_server:cast(?MODULE, {warning, Message}).	
set_level(Level, Value) ->
	gen_server:cast(?MODULE, {switch_on_off, Level, Value}).
			
%% --------------------------------------------------------------------
%% record definitions
%% --------------------------------------------------------------------
-record(state, {growl, success, warning, error }).
%% ====================================================================
%% Server functions
%% ====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

start() ->
	start_link().
%% --------------------------------------------------------------------
%% Function: init/1
%% Description: Initiates the server
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%% --------------------------------------------------------------------
init([]) ->
    {ok, #state{growl=true, success=true, warning=true, error=true}}.

%% --------------------------------------------------------------------
%% Function: handle_call/3
%% Description: Handling call messages
%% Returns: {reply, Reply, State}          |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State}               |
%%          {noreply, State, Timeout}      |
%%          {stop, Reason, Reply, State}   | (terminate/2 is called)
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_call(Request, From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%% --------------------------------------------------------------------
%% Function: handle_cast/2
%% Description: Handling cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------

handle_cast({success, Message}, State) ->
	growl_success(Message, State),
    {noreply, State};
handle_cast({error, Message}, State) ->
	growl_errors(Message, State),
    {noreply, State};
handle_cast({warning, Message}, State) ->
	growl_warnings(Message, State),
    {noreply, State};	
handle_cast({switch_on_off, Level, Value}, State) ->
	NewState = set_growl(Level, Value, State),
    {noreply, NewState};
handle_cast(Msg, State) ->
    {noreply, State}.

%% --------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_info(Info, State) ->
    {noreply, State}.

%% --------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%% --------------------------------------------------------------------
terminate(Reason, State) ->
    ok.

%% --------------------------------------------------------------------
%% Func: code_change/3
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState}
%% --------------------------------------------------------------------
code_change(OldVsn, State, Extra) ->
    {ok, State}.

%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------
set_growl(growl, true, State) ->
	growl_success("Watcherl","Notifications Enabled", State),
	State#state{growl = true};
set_growl(growl, false, State) ->
	growl_success("Watcherl","Notifications disabled", State),
	State#state{growl = false};
set_growl(success, true, State) ->
	State#state{success = true};
set_growl(success, false, State) ->
	State#state{success = false};
set_growl(warning, true, State) ->
	State#state{warning = true};	
set_growl(warning, false, State) ->
	State#state{warning = false};
set_growl(error, true, State) ->
	State#state{error = true};
set_growl(error, false, State) ->
	State#state{error = false};
set_growl(success, Value, State) ->
	error_logger:error("Value for success is not allowed : ~p~n", [Value]);
set_growl(warning, Value, State) ->
	error_logger:error("Value for warning is not allowed : ~p~n", [Value]);
set_growl(error, Value, State) ->
	error_logger:error("Value for error is not allowed : ~p~n", [Value]);
set_growl(growl, Value, State) ->
	error_logger:error("Value for growl is not allowed : ~p~n", [Value]).

get_growl(growl, State=#state{growl = Growl}) ->
	Growl;
get_growl(success, State=#state{success = Success}) ->
	Success;
get_growl(warning, State=#state{warning = Warning}) ->
	Warning;
get_growl(error, State=#state{error = Error}) ->
	Error.

growl(Image, Title, Message, State) ->
    case get_growl(growl, State) of
        false -> ok;
        true ->
            ImagePath = filename:join([filename:dirname(code:which(growler)), "..", "icons", Image]) ++ ".png",
	            Cmd = case application:get_env(watcherl, executable) of
                      undefined ->
                          case os:type() of
                              {win32, _} ->
                                  make_cmd("notifu", Image, Title, Message);
                              {unix,linux} ->
                                  make_cmd("notify-send", ImagePath, Title, Message);
                              _ ->
                                  make_cmd("growlnotify", ImagePath, Title, Message)
                          end;
                      {ok, Executable} ->
                          make_cmd(Executable, Image, Title, Message)
                  end,
            os:cmd(lists:flatten(Cmd))
    end.
	
make_cmd("growlnotify" = Util, Image, Title, Message) ->
    [Util, " -n \"Watcherl\" --image \"", Image,"\"",
     " -m \"", Message, "\" \"", Title, "\""];

make_cmd("notify-send" = Util, Image, Title, Message) ->
    [Util, " -i \"", Image, "\"",
     " \"", Title, "\" \"", Message, "\" --expire-time=5000"];
make_cmd("notifu" = Util, Image, Title, Message) ->
    %% see http://www.paralint.com/projects/notifu/
    [Util, " /q /d 5000 /t ", image2notifu_type(Image), " ",
     "/p \"", Title, "\" /m \"", Message, "\""];
	 
make_cmd(UnsupportedUtil, _, _, _) ->
    error('unsupported-sync-executable',
           lists:flatten(io_lib:format("'sync' application environment variable "
		   								"named 'executable' has unsupported value: ~p", [UnsupportedUtil]))).

image2notifu_type("success") -> "info";
image2notifu_type("warnings") -> "warn";
image2notifu_type("errors") -> "error".

growl_success(Message, State) ->
    growl_success("Success!", Message, State).

growl_success(Title, Message, State) ->	
	growl("success", Title, Message, State).

growl_errors(Message, State) ->
    growl("errors", "Errors...", Message, State).

growl_warnings(Message, State) ->
    growl("warnings", "Warnings", Message, State).

replace_chars(String, Tab) ->
    lists:map(fun (C) ->
				proplists:get_value(C, Tab, C)
              end,
    lists:flatten(String)).
%% --------------------------------------------------------------------
%%% Test functions
%% --------------------------------------------------------------------
%% --------------------------------------------------------------------
%%% Test functions
%% --------------------------------------------------------------------
-include_lib("eunit/include/eunit.hrl").
-ifdef(TEST).
-endif.