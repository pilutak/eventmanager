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

-export([
    create_subscriber/2,
    delete_subscriber/1,
    add_tel_uri/2,
    delete_tel_uri/2
    ]).

%%%===================================================================
%%% API
%%%===================================================================

create_subscriber(User, Profile) -> 
    {ok, Session} = login(sogadm, sogadm),
    send(em_interface_cai3g_envelopes:add_subscriber(Session, User, Profile)),
    {ok, _} = logout(Session).

delete_subscriber(User) -> 
    {ok, Session} = login(sogadm, sogadm),
    send(em_interface_cai3g_envelopes:delete_subscriber(Session, User)),
    {ok, _} = logout(Session).

add_tel_uri(User, E164) -> 
    {ok, Session} = login(sogadm, sogadm),
    send(em_interface_cai3g_envelopes:add_tel_uri(Session, User, E164)),
    {ok, _} = logout(Session).
    
delete_tel_uri(User, E164) -> 
    {ok, Session} = login(sogadm, sogadm),
    send(em_interface_cai3g_envelopes:delete_tel_uri(Session, User, E164)),
    {ok, _} = logout(Session).

%%%===================================================================
%%% Internal functions
%%%===================================================================

login(User, Pass)->
  em_interface_cai3g_parser:login_response(send(em_interface_cai3g_envelopes:login(User, Pass))).

logout(Session)->
  em_interface_cai3g_parser:logout_response(send(em_interface_cai3g_envelopes:logout(Session))).
    
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