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
    delete/2,
    update/2
    ]).
    

%%%===================================================================
%%% API
%%%===================================================================
create({subscriber, User, IsPsi, Pass, PubId, IRS, IsDefault, CSProfile}, #ctx{session = Session}) ->
    ?INFO_MSG("Creating HSS subscriber: ~p", [{User, IsPsi, Pass, PubId, IRS, IsDefault, CSProfile}]),
    {ok, _} = send(em_interface_cai3g_envelopes:add_subscriber(Session, User, IsPsi, Pass, PubId, IRS, IsDefault, CSProfile));
    
create({enum, Phone, PubId}, #ctx{session = Session}) ->
    ?INFO_MSG("Creating IPWorks ENUM record: ~p", [{Phone, PubId}]),
    {ok, _} = send(em_interface_cai3g_envelopes:add_enum(Session, Phone, PubId));
    
create({serviceprofile, User, PubId, CSProfile}, #ctx{session = Session}) ->
    ?INFO_MSG("Creating HSS serviceprofile: ~p", [{User, PubId, CSProfile}]),        
    {ok, _} = send(em_interface_cai3g_envelopes:add_serviceprofile(Session, User, PubId, CSProfile));
    
create({pubid, User, PubId, IRS, IsDefault, SProfile}, #ctx{session = Session}) ->
    ?INFO_MSG("Creating HSS PublicId: ~p", [{User, PubId, IRS, IsDefault, SProfile}]),
    {ok, _} = send(em_interface_cai3g_envelopes:add_pubid(Session, User, PubId, IRS, IsDefault, SProfile));
    
create({teluri, User, Phone, PubId, IRS, IsDefault}, #ctx{session = Session}) ->
    ?INFO_MSG("Creating HSS TelUri: ~p", [{User, Phone, PubId, IRS, IsDefault}]),
    {ok, _} = send(em_interface_cai3g_envelopes:add_teluri(Session, User, Phone, PubId, IRS, IsDefault)).
    
delete({enum, Phone, PubId}, #ctx{session = Session}) ->
    ?INFO_MSG("Deleting IPWorld ENUM record: ~p", [{Phone, PubId}]), 
    {ok, _} = send(em_interface_cai3g_envelopes:delete_enum(Session, Phone, PubId));

delete({subscriber, User}, #ctx{session = Session}) ->
    ?INFO_MSG("Deleting HSS subscriber: ~p", [User]), 
    {ok, _} = send(em_interface_cai3g_envelopes:delete_subscriber(Session, User));

delete({pubid, User, PubId}, #ctx{session = Session}) ->
    ?INFO_MSG("Deleting HSS PublicId: ~p", [{User, PubId}]), 
    {ok, _} = send(em_interface_cai3g_envelopes:delete_pubid(Session, User, PubId));

delete({serviceprofile, User, PubId}, #ctx{session = Session}) ->
    ?INFO_MSG("Deleting HSS serviceprofile: ~p", [{User, PubId}]), 
    {ok, _} = send(em_interface_cai3g_envelopes:delete_serviceprofile(Session, User, PubId));
    
delete({teluri, User, Phone}, #ctx{session = Session}) ->
    ?INFO_MSG("Deleting HSS Tel Uri: ~p", [{User, Phone}]), 
    {ok, _} = send(em_interface_cai3g_envelopes:delete_teluri(Session, User, Phone)).
    
update({pass, User, Pass}, #ctx{session = Session}) ->
    ?INFO_MSG("Updating HSS Password: ~p", [{User, Pass}]), 
    {ok, _} = send(em_interface_cai3g_envelopes:set_pass(Session, User, Pass)).

login(User, Pass) ->
    ?INFO_MSG("EMA Login: ~p", [{User, Pass}]),
    {ok, _} = em_interface_cai3g_parser:login_response(send(em_interface_cai3g_envelopes:login(User, Pass))).

logout(Session) ->
    ?INFO_MSG("EMA Logout: ~p", [Session]),
    {ok, _} = em_interface_cai3g_parser:logout_response(send(em_interface_cai3g_envelopes:logout(Session))).    
    
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