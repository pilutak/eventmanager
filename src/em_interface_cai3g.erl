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
-include("../include/em.hrl").

 
%% API
-export([create_subscriber/2]).
-export([delete_subscriber/1]).
-export([login/2]).
-export([send/1]).


%%%===================================================================
%%% API
%%%===================================================================

create_subscriber(UserId, ConfServiceProfile) -> 
    {ok,SessionId} = login(sogadm,sogadm),
    _Response = send(em_interface_cai3g_envelopes:env_create_hss_subscriber(SessionId, UserId, ConfServiceProfile)),
    {ok,_} = logout(SessionId).

delete_subscriber(UserId) -> 
    {ok,SessionId} = login(sogadm,sogadm),
    _Response = send(em_interface_cai3g_envelopes:env_delete_hss_subscriber(SessionId, UserId)),
    {ok,_} = logout(SessionId).

add_tel_uri(ServiceUserId, PhoneNumber) -> 
    {ok,SessionId} = login(sogadm,sogadm),
    _Response = send(em_interface_cai3g_envelopes:env_add_hss_tel_uri(SessionId, ServiceUserId, PhoneNumber)),
    {ok,_} = logout(SessionId).
    
add_sip_uri(ServiceUserId, PublicUserIdentity) -> 
    {ok,SessionId} = login(sogadm,sogadm),
    _Response = send(em_interface_cai3g_envelopes:env_add_hss_sip_uri(SessionId, ServiceUserId, PublicUserIdentity)),
    {ok,_} = logout(SessionId).

%%%===================================================================
%%% Internal functions
%%%===================================================================

login(UserId, Pass)->
  em_interface_cai3g_parser:login_response(send(em_interface_cai3g_envelopes:env_login(UserId,Pass))).

logout(SessionID)->
  em_interface_cai3g_parser:logout_response(send(em_interface_cai3g_envelopes:env_logout(SessionID))).
  
  
  
send(Request)->
    URL = "http://10.8.10.132:8998",
    Timeout = 1000,
    Delay = 5000,
    
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
