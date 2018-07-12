%% Copyright 2017 <thomas.elsgaard@timezone4.com>
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
-module(em_reader).

-export([start_link/0, init/1]).

-define(SERVER, ?MODULE).

-record(state, { socket, host}).
  
%%%===================================================================
%%% API
%%%===================================================================

      
%%%===================================================================
%%% Internal Functions
%%%===================================================================
start_link() ->
    proc_lib:start_link(?MODULE, init, [self()]).

init(Parent) ->
    process_flag(trap_exit, true),
    ok = proc_lib:init_ack(Parent, {ok, self()}),
    econfig:register_config(em, ["/etc/em/em.ini"], [autoreload]),
    Host = econfig:get_value(em, "broadworks", "host"),
    connect(#state{socket = undefined, host = Host}).

connect(State=#state{host=Host}) ->
    case gen_tcp:connect(Host, 8025, [{buffer, 65536},{active, once},{packet, line}], 10000) of
        {ok, Sock} ->
        logger:notice("Socket connected: ~p", [Host]),
	    loop(State#state{socket=Sock});

        {error,timeout} ->
        logger:error("Socket timeout, reconnecting: ~p", [Host]),
	    connect(State);

        {error,Reason} ->
	    logger:error("Socket timeout, reconnecting: ~p", [Reason]),
	    timer:sleep(10000),
	    connect(State)

    end.
 
% Main loop
loop(State) ->
    receive
	{tcp, Sock, Data} ->
	    inet:setopts(Sock,[{active,once}]),
	    process(Data,State);

	{tcp_closed, _} ->
	    logger:error("Socket closed, reconnecting"),
	    timer:sleep(10000),
	    connect(State);

	{tcp_error, _, Reason} ->
	    logger:error("Socket error: ~s", [Reason]);

	Any when is_tuple(Any), is_pid(element(2, Any)) ->
	    element(2, Any) ! {em_error, self(), {notowner,
	    "Operations are restricted to the owner of the connection."}},
	    loop(State);
    
	Any ->
	    %error_logger:error_msg("Unexpected message: ~w~n", [Any]),
        logger:error("Unexpected message: ~p", [Any]),
	    loop(State)
    end.
  
process("<?xml version=\"1.0\" encoding=\"ISO-8859-1\"?>\n", State) ->
    loop(State);

process(Data, State) ->
    em_event_server:process_event(Data),
    loop(State).