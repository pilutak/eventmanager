%% Copyright 2017 <thomas.elsgaard@middleaware.dk>
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
-module(em_event_server).

-behaviour(gen_server).

%% API
-export([start_link/0]).
-export([process_event/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {}).

%%%===================================================================
%%% API
%%%===================================================================
process_event(Event) ->
    gen_server:call(?SERVER, {process_event, Event}).

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    process_flag(trap_exit, true),
    {ok, #state{}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call({process_event, Event}, _From, State) ->
    {UserId, CommandType, Message} = top_parser(Event),
    
    % We have to verify if the userId exists since some commands do not contain userId,
    % when missing, the userId from the top level document is used.
    case UserId of
        undefined -> 
            Id = persist_event(get_user_id(Event), CommandType, Event),    
            Reply = em_event:process(Id, CommandType, Message),
            {reply, Reply, State, infinity};
        _->
            Id = persist_event(UserId, CommandType, Event),    
            Reply = em_event:process(Id, CommandType, Message),
            {reply, Reply, State, infinity}
    end;


handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
top_parser(Data) -> 
    {ParsedMessage,_} = xmerl_scan:string(Data),
    parser_message(em_utils:get_type_message(ParsedMessage), ParsedMessage).

parser_message('BroadsoftOCIReportingDocument', Data)->
    [Command|_Other_ignored]=em_utils:get_elements(command,em_utils:get_element_childs(Data)),
    [User] = em_utils:get_elements(userId, em_utils:get_element_childs(Command)),
    UserId = em_utils:get_element_text(User),
    
    % We are ignoring all events coming from the AS itself, this is events
    % from the user *XS localhost Admin*.
    case UserId of 
        "*XS localhost Admin*" -> {ignored, ignored, undefined};
        _->
            case em_utils:get_cdata(Data) of
	            [] ->
	                {ignored, ignored, undefined};
	            CDATA ->
	                {ParsedCDATA,_} = xmerl_scan:string(CDATA),
	                parser_message(em_utils:get_type_message(ParsedCDATA), ParsedCDATA)
            end
    end;

parser_message('BroadsoftDocument', Data)->
    [Command|_Other_ignored]=em_utils:get_elements(command,em_utils:get_element_childs(Data)),
    [User|_Other_ignored]=em_utils:get_elements(userId,em_utils:get_element_childs(Data)),
    %%%===================================================================
    %%% We split the string from em_utils:get_element_attributes('xsi:type',Command) eg. UserAddRequest17sp4 to two variables CommandType and _CommandType_ignore
    %%% by using "string:take(String, lists:seq($0,$9), true, leading),"
    %%% CommandType=UserAddRequest
    %%% _CommandType_ignore=17sp4
    %%% em_event.erl commandlist needs to be updated accordingly.
    %%%===================================================================
    {CommandType,_CommandType_ignore} = string:take(em_utils:get_element_attributes('xsi:type',Command), lists:seq($0,$9), true, leading),
    UserId = em_utils:get_element_text(User),
    {UserId, CommandType, Command};
    
parser_message(_,_)->
    {ignored, ignored, undefined}.
    
persist_event(_ ,ignored, undefined) ->
    ignored;
persist_event(_ ,ignored, _) ->
    ignored;
persist_event(UserId, CommandType, Data) ->
    logger:debug("Persisting event: ~p, ~p, ~p", [UserId, CommandType, Data]),
    em_db:insert_event(UserId, CommandType, Data).
    
get_user_id(Event) ->
    {ParsedMessage,_} = xmerl_scan:string(Event),
    [Command|_Other_ignored]=em_utils:get_elements(command,em_utils:get_element_childs(ParsedMessage)),
    [User] = em_utils:get_elements(userId, em_utils:get_element_childs(Command)),
    em_utils:get_element_text(User).
