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
    ?INFO_MSG("Creating HSS subscriber: ~p", [{User, CSProfile, SProfile}]),
    send(em_interface_cai3g_envelopes2:add_subscriber(Session, User, CSProfile, SProfile));
    
create({enum, Phone, PubId}, #ctx{session = Session}) ->
    ?INFO_MSG("Creating IPWorks ENUM record: ~p", [{Phone, PubId}]),
    send(em_interface_cai3g_envelopes2:add_enum(Session, Phone, PubId));
    
create({serviceprofile, User, PubId, CSProfile}, #ctx{session = Session}) ->
    ?INFO_MSG("Creating HSS serviceprofile: ~p", [{User, PubId, CSProfile}]),        
    send(em_interface_cai3g_envelopes2:add_serviceprofile(Session, User, PubId, CSProfile));
    
create({pubid, User, PubId, SProfile}, #ctx{session = Session}) ->
    ?INFO_MSG("Creating HSS PublicId: ~p", [{User, PubId, SProfile}]),
    send(em_interface_cai3g_envelopes2:add_pubid(Session, User, PubId, SProfile));
    
create({teluri, User, Phone, PubId}, #ctx{session = Session}) ->
    ?INFO_MSG("Creating HSS TelUri: ~p", [{User, Phone, PubId}]),
    send(em_interface_cai3g_envelopes2:add_teluri(Session, User, Phone, PubId)).
    
delete({enum, Phone, PubId}, #ctx{session = Session}) ->
    ?INFO_MSG("Deleting IPWorld ENUM record: ~p", [{Phone, PubId}]), 
    send(em_interface_cai3g_envelopes2:delete_enum(Session, Phone, PubId));

delete({subscriber, User}, #ctx{session = Session}) ->
    ?INFO_MSG("Deleting HSS subscriber: ~p", [User]), 
    send(em_interface_cai3g_envelopes2:delete_subscriber(Session, User));

delete({pubid, User, PubId}, #ctx{session = Session}) ->
    ?INFO_MSG("Deleting HSS PublicId: ~p", [{User, PubId}]), 
    send(em_interface_cai3g_envelopes2:delete_pubid(Session, User, PubId));

delete({serviceprofile, User, PubId}, #ctx{session = Session}) ->
    ?INFO_MSG("Deleting HSS serviceprofile: ~p", [{User, PubId}]), 
    send(em_interface_cai3g_envelopes2:delete_serviceprofile(Session, User, PubId));
    
delete({teluri, User, Phone}, #ctx{session = Session}) ->
    ?INFO_MSG("Deleting HSS Tel Uri: ~p", [{User, Phone}]), 
    send(em_interface_cai3g_envelopes2:delete_teluri(Session, User, Phone)).

login(User, Pass) ->
    ?INFO_MSG("EMA Login: ~p", [{User, Pass}]),
    em_interface_cai3g_parser:login_response(send(em_interface_cai3g_envelopes:login(User, Pass))).

logout(Session) ->
    ?INFO_MSG("EMA Logout: ~p", [Session]),
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