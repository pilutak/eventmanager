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

-module(em_hss).
-include("../include/em.hrl").

-export([
    login/2,
    logout/1,
    create/2,
    delete/2
    ]).
    

%%%===================================================================
%%% API
%%%===================================================================
create({subscriber, User, CSProfile, SProfile}, #ctx{session = Session}) ->
    send(em_interface_cai3g_envelopes2:add_subscriber(Session, User, CSProfile, SProfile));
    
create({enum, Phone, PubId}, #ctx{session = Session}) ->
    send(em_interface_cai3g_envelopes2:add_enum(Session, Phone, PubId));
    
create({serviceprofile, UserName, PubId, CSProfile}, #ctx{session = Session}) ->        
    send(em_interface_cai3g_envelopes2:add_serviceprofile(Session, UserName, PubId, CSProfile));
    
create({pubid, User, PubId, SProfile}, #ctx{session = Session}) ->
    send(em_interface_cai3g_envelopes2:add_pubid(Session, User, PubId, SProfile));
    
create({teluri, UserName, Phone, PubId}, #ctx{session = Session}) ->
    send(em_interface_cai3g_envelopes2:add_teluri(Session, UserName, Phone, PubId)).
    
delete({enum, Phone, PubId}, #ctx{session = Session}) -> 
    send(em_interface_cai3g_envelopes2:delete_enum(Session, Phone, PubId));

delete({subscriber, UserName}, #ctx{session = Session}) -> 
    send(em_interface_cai3g_envelopes2:delete_subscriber(Session, UserName));

delete({pubid, UserName, PubId}, #ctx{session = Session}) -> 
    send(em_interface_cai3g_envelopes2:delete_pubid(Session, UserName, PubId));

delete({serviceprofile, UserName, PubId}, #ctx{session = Session}) -> 
    send(em_interface_cai3g_envelopes2:delete_serviceprofile(Session, UserName, PubId));
    
delete({teluri, User, Phone}, #ctx{session = Session}) -> 
    send(em_interface_cai3g_envelopes2:delete_teluri(Session, User, Phone)).

login(User, Pass) ->
    em_interface_cai3g_parser:login_response(send(em_interface_cai3g_envelopes:login(User, Pass))).

logout(Session) ->
      em_interface_cai3g_parser:logout_response(send(em_interface_cai3g_envelopes:logout(Session))).    
    
%%%===================================================================
%%% Internal functions
%%%===================================================================    
send(Request) ->
    %{ok, {Url, _, _}} = application:get_env(em, ema),
  case httpc:request(post,{?EMA_URL,[],"text/xml",list_to_binary(Request)},[{timeout,1000}],[]) of
    {ok,{{_,200,_}, _Headers, Body}}-> {ok, Body};
    {ok,{{_,500,_}, _Headers, Body}}-> {error, Body};
    {ok,{{_,_OtherStatus,_},_Headers,_Body}}->
      %?LOG("HTTP Status Code unexpected ~p ~n waiting ~p miliseconds ~n",OtherStatus, 5000),
      timer:sleep(5000),
      exit(http_code_unexpected);
    {error,Reason} ->
      %?LOG("Error on sending to EMA ~p ~n waiting ~p miliseconds ~n",Reason, 5000),
      timer:sleep(5000),
      exit(Reason)
  end.