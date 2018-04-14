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

-record(state, { socket, host, ema_url, ema_user, ema_pass }).
  
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
    ?INFO_MSG("You are my creator, but I am your master; - obey! ~n", []),	
    connect(#state{socket = undefined, host = Host}).


connect(State=#state{host=Host}) ->
    case gen_tcp:connect(Host, 8025, [{buffer, 32768},{active, once},{packet, line}], 10000) of
        {ok, Sock} ->
	    %{ok,Dir}=file:get_cwd(),
        ?INFO_MSG("Socket connected: ~p~n", [Host]),
	    %?LOG("Connected (em2.log located at ~p ) ~n",Dir),
	    loop(State#state{socket=Sock});

        {error,timeout} ->
        ?ERROR_MSG("Socket timeout, reconnecting: ~p~n", [Host]),
	    connect(State);

        {error,Reason} ->
	    ?ERROR_MSG("Socket timeout, reconnecting: ~p~n", [Reason]),
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
	    ?ERROR_MSG("Socket closed, reconnecting: ~n", []),
	    timer:sleep(10000),
	    connect(State);

	{tcp_error, _, Reason} ->
	    ?ERROR_MSG("Socket error: ~p~n", [Reason]);


	Any when is_tuple(Any), is_pid(element(2, Any)) ->
	    element(2, Any) ! {em_error, self(), {notowner,
	    "Operations are restricted to the owner of the connection."}},
	    loop(State);
    
	Any ->
	    %error_logger:error_msg("Unexpected message: ~w~n", [Any]),
        ?ERROR_MSG("Unexpected message: ~p~n", [Any]),
	    loop(State)
    end.
  

%% Parsing and dispatching of events
process("<?xml version=\"1.0\" encoding=\"ISO-8859-1\"?>\n", State) ->
    loop(State);

process(Data, State) ->
    {UserId, CommandType, Message} = top_parser(Data),
    
    Id = persist_event(maps:is_key(CommandType, em_events:processors()), UserId, CommandType, Data),
    Pid = spawn_link(em_events, process, [Id, CommandType, Message]),
    await_result(Pid, Id),
    loop(State).

top_parser(Data) -> 
    {ParsedMessage,_} = xmerl_scan:string(Data),
    parser_message(em_utils:get_type_message(ParsedMessage), ParsedMessage).

parser_message('BroadsoftOCIReportingDocument', Data)->
    case em_utils:get_cdata(Data) of
	[] ->
	    {ignored, ignored,undefined};
	CDATA ->
	    {ParsedCDATA,_} = xmerl_scan:string(CDATA),
	    parser_message(em_utils:get_type_message(ParsedCDATA), ParsedCDATA)
    end;

parser_message('BroadsoftDocument', Data)->
    [Command|_Other_ignored]=em_utils:get_elements(command,em_utils:get_element_childs(Data)),
    [User|_Other_ignored]=em_utils:get_elements(userId,em_utils:get_element_childs(Data)),
    CommandType = em_utils:get_element_attributes('xsi:type',Command),
    UserId = em_utils:get_element_text(User),
    {UserId, CommandType, Command};
    
    %{em_utils:get_element_attributes('xsi:type',Command),Command};

parser_message(_,_)->
    {ignored, ignored, undefined}.
    
    
await_result(Pid,Id) ->
    receive
        {'EXIT', Pid, normal} ->
            %?ERROR_MSG("EXIT normal: ~p", [Pid]), 
            ok;
        {'EXIT', Pid, shutdown} -> 
            %?ERROR_MSG("EXIT shutdown: ~p", [Pid]),
            ok;
        {'EXIT', Pid, _ } -> 
            %?ERROR_MSG("EXIT other: ~p, ~p", [Pid, Reason]),
            case Id of
                ignored -> ok;
                Id -> fail_event(Id)
            end,
            ok
    after 8000 ->
            %?ERROR_MSG("Event process timeout: ~p", [Pid]),
            timeout
    end.

persist_event(_ ,_ ,ignored, undefined) ->
    ignored;
persist_event(_,_ ,ignored, _) ->
    ignored;
persist_event(true, UserId, CommandType, Data) ->
    em_db:insert_event(UserId, CommandType, Data);
    %?INFO_MSG("EVENT: ~p, ~p, ~p ~n", [UserId, CommandType, Data]).
persist_event(false, UserId, CommandType, Data) ->
    em_db:insert_white_event(UserId, CommandType, Data).
    %?INFO_MSG("EVENT: ~p, ~p, ~p ~n", [UserId, CommandType, Data]).


    
fail_event(Id) ->
    em_db:fail_event(Id).
    %?INFO_MSG("EVENT: ~p, ~p, ~p ~n", [UserId, CommandType, Data]).

            
