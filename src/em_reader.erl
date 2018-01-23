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

-export([
    start_connection/0,
    start_link/1,
    init/2]).

-include("../include/em.hrl").

-record(state0, { socket, host, ema_url, ema_user, ema_pass }).
  
%%%===================================================================
%%% API
%%%===================================================================

start_connection() ->
{ok, Hosts} = application:get_env(em, bw_hosts),
    lists:foreach(
      fun(I) ->
	      {ok, _} = supervisor:start_child(em_reader_sup, [I])
      end, Hosts).

      
%%%===================================================================
%%% Internal Functions
%%%===================================================================

start_link(Host) ->
    proc_lib:start_link(?MODULE, init, [self(), Host]).


init(Parent, Host) ->
    process_flag(trap_exit, true),
    ok = proc_lib:init_ack(Parent, {ok, self()}),
    register(list_to_atom(Host),self()),
    connect(#state0{socket = undefined, host = Host, ema_url = ?EMA_URL, ema_user = ?EMA_USER, ema_pass = ?EMA_PASS}).


connect(State=#state0{host=Host}) ->
    case gen_tcp:connect(Host, 8025, [{buffer, 32768},{active, once},{packet, line}], 10000) of
        {ok, Sock} ->
	    %{ok,Dir}=file:get_cwd(),
        ?INFO_MSG("Socket connected: ~p", [Host]),
	    %?LOG("Connected (em2.log located at ~p ) ~n",Dir),
	    loop(State#state0{socket=Sock});

        {error,timeout} ->
        ?ERROR_MSG("Socket timeout, reconnecting: ~p", [Host]),
	    connect(State);

        {error,Reason} ->
	    ?ERROR_MSG("Socket timeout, reconnecting: ~p", [Reason]),
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
	    ?ERROR_MSG("Socket closed, reconnecting: ~p", []),
	    timer:sleep(10000),
	    connect(State);

	{tcp_error, _, Reason} ->
	    ?ERROR_MSG("Socket error: ~p", [Reason]);


	Any when is_tuple(Any), is_pid(element(2, Any)) ->
	    element(2, Any) ! {em_error, self(), {notowner,
	    "Operations are restricted to the owner of the connection."}},
	    loop(State);
    
	Any ->
	    %error_logger:error_msg("Unexpected message: ~w~n", [Any]),
        ?ERROR_MSG("Unexpected message: ~w~n", [Any]),
	    loop(State)
    end.
  

%% Parsing and dispatching of events
process("<?xml version=\"1.0\" encoding=\"ISO-8859-1\"?>\n", State) ->
    loop(State);

process(Data, State) ->
    {CommandType, Message} = top_parser(Data),
    
    Pid = spawn_link(em_events, process, [CommandType, Message]),
    await_result(Pid),
    loop(State).

top_parser(Data) -> 
    {ParsedMessage,_} = xmerl_scan:string(Data),
    parser_message(em_utils:get_type_message(ParsedMessage), ParsedMessage).

parser_message('BroadsoftOCIReportingDocument', Data)->
    case em_utils:get_cdata(Data) of
	[] ->
	    {ignored,undefined};
	CDATA ->
	    {ParsedCDATA,_} = xmerl_scan:string(CDATA),
	    parser_message(em_utils:get_type_message(ParsedCDATA), ParsedCDATA)
    end;

parser_message('BroadsoftDocument', Data)->
    [Command|_Other_ignored]=em_utils:get_elements(command,em_utils:get_element_childs(Data)),
    {em_utils:get_element_attributes('xsi:type',Command),Command};

parser_message(_,_)->
    {ignored,undefined}.
    
    
await_result(Pid) ->
    receive
        {'EXIT', Pid, normal} -> 
            ok;
        {'EXIT', Pid, shutdown} -> 
            ok;
        {'EXIT', Pid, _ } -> 
            ok
    after 8000 ->
            ?ERROR_MSG("Event process timeout: ~p", [Pid]),
            timeout
    end.
