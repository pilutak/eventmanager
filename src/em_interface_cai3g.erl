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
    create_subscriber/3,
    delete_subscriber/2,
    add_tel_uri/3,
    delete_tel_uri/3
    ]).
    
-record(state, { socket, host, ema_url, ema_user, ema_pass }).

%%%===================================================================
%%% API
%%%===================================================================

create_subscriber(User, Profile, State) -> 
    {ok, Session} = login(State),
    send(em_interface_cai3g_envelopes:add_subscriber(Session, User, Profile), State),
    {ok, _} = logout(Session, State).

delete_subscriber(User, State) -> 
    {ok, Session}  = login(State),
    send(em_interface_cai3g_envelopes:delete_subscriber(Session, User), State),
    {ok, _} = logout(Session, State).

add_tel_uri(User, E164, State) -> 
    {ok, Session} = login(State),
    send(em_interface_cai3g_envelopes:add_tel_uri(Session, User, E164), State),
    {ok, _} = logout(Session, State).
    
delete_tel_uri(User, E164, State) -> 
    {ok, Session}  = login(State),
    send(em_interface_cai3g_envelopes:delete_tel_uri(Session, User, E164), State),
    {ok, _} = logout(Session, State).

%%%===================================================================
%%% Internal functions
%%%===================================================================

login(#state{ema_user = User, ema_pass = Pass} = State) ->
    em_interface_cai3g_parser:login_response(send(em_interface_cai3g_envelopes:login(User, Pass), State)).

logout(Session, State) ->
      em_interface_cai3g_parser:logout_response(send(em_interface_cai3g_envelopes:logout(Session), State)).
    
send(Request, #state{ema_url= EMAUrl}) ->
    %{ok, {Url, _, _}} = application:get_env(em, ema),
  case httpc:request(post,{EMAUrl,[],"text/xml",list_to_binary(Request)},[{timeout,1000}],[]) of
    {ok,{{_,200,_},_Headers,Body}}-> {ok,Body};
    {ok,{{_,500,_},_Headers,Body}}-> {error,Body};
    {ok,{{_,_OtherStatus,_},_Headers,_Body}}->
      %?LOG("HTTP Status Code unexpected ~p ~n waiting ~p miliseconds ~n",OtherStatus, 5000),
      timer:sleep(5000),
      exit(http_code_unexpected);
    {error,Reason} ->
      %?LOG("Error on sending to EMA ~p ~n waiting ~p miliseconds ~n",Reason, 5000),
      timer:sleep(5000),
      exit(Reason)
  end.