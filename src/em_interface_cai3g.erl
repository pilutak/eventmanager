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

-module(em_interface_cai3g).

-behaviour(gen_server).

%% API
-export([start_link/0]).
-import(em_interface_cai3g_envelopes,[env_login/2,env_create_hss_subscriber/2,env_logout/1,env_add_hss_tel_uri/3]).
-import(em_interface_cai3g_envelopes,[env_add_hss_sip_uri/3,env_delete_hss_subscriber/2]).
-import(em_interface_cai3g_parser,[login_response/1,logout_response/1]).
-include("../include/em.hrl").

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {
  ema_url,
  ema_user,
  ema_pass,
  reconnect_delay,
  timeout
}).

%%%===================================================================
%%% API
%%%===================================================================


start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init([]) ->
  {ok, #state{ema_url = ?EMA_URL,ema_user = ?EMA_UserID, ema_pass = ?EMA_Pass, reconnect_delay = ?RECONNECT_DELAY, timeout = ?EMA_CONNECTION_TIMEOUT }}.

handle_call({create_subscriber,UserId}, _From, State) ->
  {ok,SessionId} = login(State),
  Response =send(env_create_hss_subscriber(SessionId,UserId),State),
  {ok,SessionId} = logout(SessionId,State),
  {reply, Response, State};


handle_call({add_telURI,ServiceUserId,PhoneNumber}, _From, State) ->
  {ok,SessionId} = login(State),
  Response =send(env_add_hss_tel_uri(SessionId,ServiceUserId,PhoneNumber),State),
  {ok,SessionId} = logout(SessionId,State),
  {reply, Response, State};

handle_call({add_sipURI,ServiceUserId,PublicUserIdentity}, _From, State) ->
  {ok,SessionId} = login(State),
  Response =send(env_add_hss_sip_uri(SessionId,ServiceUserId,PublicUserIdentity),State),
  {ok,SessionId} = logout(SessionId,State),
  {reply, Response, State};

handle_call({del_user,ServiceUserId}, _From, State) ->
  {ok,SessionId} = login(State),
  Response =send(env_delete_hss_subscriber(SessionId,ServiceUserId),State),
  {ok,SessionId} = logout(SessionId,State),
  {reply, Response, State};


handle_call(_Request, _From, State) ->
  {reply, ok, State}.

handle_cast(_Request, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

login(#state{ema_user = UserId, ema_pass = Pass}=State)->
  login_response(send(env_login(UserId,Pass),State)).

logout(SessionID,State)->
  logout_response(send(env_logout(SessionID),State)).

send(Request,#state{ema_url = URL,reconnect_delay = Delay, timeout = Timeout})->
  case httpc:request(post,{URL,[],"text/xml",list_to_binary(Request)},[{timeout,Timeout}],[]) of
    {ok,{{_,200,_},_Headers,Body}}-> {ok,Body};
    {ok,{{_,500,_},_Headers,Body}}-> {error,Body};
    {ok,{{_,OtherStatus,_},_Headers,_Body}}->
      ?LOG("HTTP Status Code unexpected ~p ~n waiting ~p miliseconds ~n",OtherStatus,Delay),
      timer:sleep(Delay),
      exit(http_code_unexpected);
    {error,Reason} ->
      ?LOG("Error on sending to EMA ~p ~n waiting ~p miliseconds ~n",Reason,Delay),
      timer:sleep(Delay),
      exit(Reason)
  end.

