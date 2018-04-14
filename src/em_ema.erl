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

-module(em_ema).
-behaviour(gen_server).
-include("../include/em.hrl").

%% API
-export([start_link/0]).
-export([request/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {ema_hosts, session}).

%%%===================================================================
%%% API
%%%===================================================================

request(SoapEnv) ->
    gen_server:call(?SERVER, {send_request, SoapEnv}).
    
    
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

    {ok, Args} = application:get_env(em, em_ema),
    {ok, #state{ema_hosts = Args}}.

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
handle_call({send_request, SoapEnv}, _From, State) ->
    
    {Resource, SessionId} = open_session(State),    
    Request = em_auth:add_session_id(SessionId, SoapEnv),
    case send(Request, Resource) of
        {ok, _} ->
            close_session(SessionId, Resource),
            {reply, ok, State};
        % It is OK to delete a non existing association
        {error, {"4006", "13005"}} -> 
            close_session(SessionId, Resource),
            {reply, ok, State};
        {error, Err} -> exit(Err)
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
open_session(State=#state{ema_hosts = Hosts}) ->
    
    Primary = proplists:get_value(primary, Hosts),
    Hostname = proplists:get_value(hostname, Primary),
    Port = proplists:get_value(port, Primary),
    Url = proplists:get_value(url, Primary),
    Username = proplists:get_value(username, Primary),
    Password = proplists:get_value(password, Primary),
    Resource = "http://" ++ Hostname ++ ":" ++ Port ++ Url, 
    
    SoapEnv = em_cai3g_envelope:login(Username, Password),
    case send(SoapEnv, Resource) of
        {ok, Body} ->
            {ok, SessionId} = em_interface_cai3g_parser:login_response({ok, Body}),
            ?INFO_MSG("EMA session created on primary EMA: ~p~n", [SessionId]),
            {Resource, SessionId};
        {error, _} -> 
            ?INFO_MSG("EMA session retrying on secondary EMA: ~n", []),
            open_session(secondary, State)
    end.
    
open_session(secondary, #state{ema_hosts = Hosts}) ->
    
    Secondary = proplists:get_value(secondary, Hosts),
    Hostname = proplists:get_value(hostname, Secondary),
    Port = proplists:get_value(port, Secondary),
    Url = proplists:get_value(url, Secondary),
    Username = proplists:get_value(username, Secondary),
    Password = proplists:get_value(password, Secondary),
    Resource = "http://" ++ Hostname ++ ":" ++ Port ++ Url, 
    
    SoapEnv = em_cai3g_envelope:login(Username, Password),
    case send(SoapEnv, Resource) of
        {ok, Body} ->
            {ok, SessionId} = em_interface_cai3g_parser:login_response({ok, Body}),
            ?INFO_MSG("EMA session created on secondary EMA: ~p~n", [SessionId]),
            {Resource, SessionId};
        {error, Reason} -> 
            ?ERROR_MSG("Giving up contactig EMA (tried both primary and secodary): ~n", []),
            exit(Reason)
    end.
                
close_session(Session, Host) ->
    {ok, _} = em_interface_cai3g_parser:logout_response(send(em_cai3g_envelope:logout(Session), Host)).
  
send(Request, Host) ->
    %?INFO_MSG("Sending request: ~p", [lists:flatten(Request)]),
    case httpc:request(post,{Host,[],"text/xml",list_to_binary(Request)},[{timeout,1000}],[]) of
        {ok,{{_,200,_}, _Headers, Body}}-> {ok, Body};
        {ok,{{_,500,_}, _Headers, Body}}-> em_interface_cai3g_parser:error_resp(Body);
        %{ok,{{_,500,_}, _Headers, Body}}-> {error, Body};
    
        {ok,{{_,_OtherStatus,_},_Headers,_Body}} ->
            %?LOG("HTTP Status Code unexpected ~p ~n waiting ~p miliseconds ~n",OtherStatus, 5000),
            exit(http_code_unexpected);
        {error, {failed_connect, Error}} ->
            ?INFO_MSG("Connect failed towards EMA: ~p~n", [Host]),
            {error, Error};
        {error,Reason} ->
            %?INFO_MSG("Error towards EMA: ~p", [Reason]),
            exit(Reason)
    end. 

