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

-module(em_interface_ocir).

%% API.
-export([start_connection/0]).
%-export([close/1]).

%% Internals.
-export([start_link/1]).
-export([init/2]).

-include("../include/em.hrl").

-record(state, { socket, host }).
  

start_connection() ->
{ok, Hosts} = application:get_env(em, bw_hosts),
    lists:foreach(
      fun(I) ->
	      {ok, _} = supervisor:start_child(em_interface_ocir_sup, [I])
      end, Hosts).

      
%%%===================================================================
%%% Internal Functions
%%%===================================================================

start_link(Host) ->
    proc_lib:start_link(?MODULE, init, [self(), Host]).


init(Parent, Host) ->
    ok = proc_lib:init_ack(Parent, {ok, self()}),
    register(list_to_atom(Host),self()),
    connect(#state{socket = undefined, host = Host}).


connect(State=#state{host=Host}) ->
    case gen_tcp:connect(Host, 32768, [{buffer, 8192},{active, once},{packet, line}], 10000) of
        {ok, Sock} ->
	    {ok,Dir}=file:get_cwd(),
	    ?LOG("Connected (em2.log located at ~p ) ~n",Dir),
	    loop(State#state{socket=Sock});

        {error,timeout} ->
	    ?LOG("TIMEOUT: Reconnecting.... ~n"),
	    connect(State);

        {error,Reason} ->
	    ?LOG("Error ~p ~n Reconnecting... ~n",Reason),
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
	    io:format("TCP is closed"),
	    timer:sleep(10000),
	    connect(State);

	{tcp_error, _, Reason} ->
	    io:format(Reason);


	Any when is_tuple(Any), is_pid(element(2, Any)) ->
	    element(2, Any) ! {em_error, self(), {notowner,
	    "Operations are restricted to the owner of the connection."}},
	    loop(State);
    
	Any ->
	    error_logger:error_msg("Unexpected message: ~w~n", [Any]),
	    loop(State)
    end.
  

%% Parsing and dispatching of events
process("<?xml version=\"1.0\" encoding=\"ISO-8859-1\"?>\n", State) ->
    loop(State);

process(Data, State) ->
    {Command_type, Message} = top_parser(Data),
    em_events:process(Command_type,Message,Data),
    loop(State).

top_parser(Data) -> 
    {Parsed_Message,_} = xmerl_scan:string(Data),
    parser_message(em_utils:get_type_message(Parsed_Message),Parsed_Message).

parser_message('BroadsoftOCIReportingDocument',Data)->
    case em_utils:get_cdata(Data) of
	[] ->
	    {ignored,undefined};
	CDATA ->
	    {Parsed_CDATA,_} = xmerl_scan:string(CDATA),
	    parser_message(em_utils:get_type_message(Parsed_CDATA),Parsed_CDATA)
    end;

parser_message('BroadsoftDocument',Data)->
    [Command|_Other_ignored]=em_utils:get_elements(command,em_utils:get_element_childs(Data)),
    {em_utils:get_element_attributes('xsi:type',Command),Command};

parser_message(_,_)->
    {ignored,undefined}.
