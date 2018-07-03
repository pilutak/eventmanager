%%% Copyright 2017 <thomas.elsgaard@timezone4.com>
%%%
%%% Licensed under the Apache License, Version 2.0 (the "License");
%%% you may not use this file except in compliance with the License.
%%% You may obtain a copy of the License at
%%%
%%%   http://www.apache.org/licenses/LICENSE-2.0
%%%
%%% Unless required by applicable law or agreed to in writing, software
%%% distributed under the License is distributed on an "AS IS" BASIS,
%%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%% See the License for the specific language governing permissions and
%%% limitations under the License.

-module(em_surgemail).
-behaviour(gen_server).
-include("../include/em.hrl").

%% API
-export([start_link/0]).
-export([request/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {}).

%%%===================================================================
%%% API
%%%===================================================================

request(Request) ->
    gen_server:call(?SERVER, {send_request, Request}).
    
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
    econfig:register_config(em, ["/etc/em/em.ini"], [autoreload]),
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
handle_call({send_request, Request}, _From, State) ->
    {ok, _} = send(Request),
    {reply, ok, State};
        
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
  
send(Request) ->
    ?INFO_MSG("Sending request to primary Surgemail ~n", []), 
    Hostname = econfig:get_value(em, "surgemail", "primary_host"),
    Port = econfig:get_value(em, "surgemail", "port"),
    Username = econfig:get_value(em, "surgemail", "username"),
    Password = econfig:get_value(em, "surgemail", "password"),
    
    Url = "http://" ++ Hostname ++ ":" ++ Port ++ Request, 
    %?INFO_MSG("Surgemail URL: ~p~n", [Url]), 

    ContentType = "text/html",
    Headers = [auth_header(Username, Password), {"Content-Type", ContentType}],
    case httpc:request(post, {Url, Headers, ContentType, ""}, [{timeout,4000}], []) of
            {ok,{{_,200,_}, _Headers, Body}}-> {ok, Body};
            {ok,{{_,_OtherStatus,_},_Headers,_Body}} ->
                exit(http_code_unexpected);
            {error, {failed_connect, _Error}} ->
                ?INFO_MSG("Connect failed towards primary surgemail: ~p~n", [Hostname]),
                send_secondary(Request);
            {error, Reason} ->
                exit(Reason)
    end. 

send_secondary(Request) ->
    ?INFO_MSG("Sending request to secondary Surgemail ~n", []), 
    Hostname = econfig:get_value(em, "surgemail", "secondary_host"),
    Port = econfig:get_value(em, "surgemail", "port"),
    Username = econfig:get_value(em, "surgemail", "username"),
    Password = econfig:get_value(em, "surgemail", "password"),
    Url = "http://" ++ Hostname ++ ":" ++ Port ++ Request, 
    %?INFO_MSG("Surgemail URL: ~p~n", [Url]), 

    ContentType = "text/html",
    Headers = [auth_header(Username, Password), {"Content-Type", ContentType}],
    case httpc:request(post, {Url, Headers, ContentType, ""}, [{timeout,4000}], []) of
            {ok,{{_,200,_}, _Headers, Body}}-> {ok, Body};
            {ok,{{_,_OtherStatus,_},_Headers,_Body}} ->
                exit(http_code_unexpected);
            {error, {failed_connect, Error}} ->
                ?INFO_MSG("Connect failed towards secondary surgemail: ~p~n", [Hostname]),
                {error, Error};
            {error, Reason} ->
                exit(Reason)
    end. 
        
    
auth_header(Username, Password) ->
    Encoded = base64:encode_to_string(lists:append([Username,":",Password])),
    {"Authorization","Basic " ++ Encoded}.
 


