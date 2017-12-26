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

-module(em_processor_user).

-export([create/1, delete/1, modify/1]).
-include("../include/em.hrl").

%%%===================================================================
%%% API
%%%===================================================================
create(Event) ->
    State=#state{session=em_ema_session:open()},
    em_hss:create({subscriber, Event}, State),
    {ok, _} = em_srd:add_user(Event),
    em_ema_session:close(State).
        
    
delete(#event{user=UserName}) ->
    CurrentPubId = em_srd:get_sipuri(UserName),
    CurrentPhone = em_srd:get_e164(UserName),

    State=#state{session=em_ema_session:open()},
    
        case CurrentPhone of
            undefined ->
               em_hss:delete({subscriber, UserName}, State),
               {ok, _} = em_srd:delete_user(UserName);
           
            _ ->
                em_hss:delete({enum, CurrentPhone, CurrentPubId}, State),
                em_hss:delete({subscriber, UserName}, State),
                {ok, _} = em_srd:delete_user(UserName)
        end,
    em_ema_session:close(State).   


modify(Event=#event{user=UserName, type='user'}) ->
    State=#state{session=em_ema_session:open()},

    CurrentPubId = em_srd:get_sipuri(UserName),
    CurrentPhone = em_srd:get_e164(UserName),
    
    PlannedChanges = plan_change(Event),
    ?INFO_MSG("Executing plan: ~p", [PlannedChanges]),
    
    case PlannedChanges of
        
        {none, none} ->
            ignore;
        {none, delete} ->
            em_hss:delete({enum, CurrentPhone, CurrentPubId}, State),
            em_hss:delete({teluri, UserName, CurrentPhone}, State),
            em_srd:delete_e164(UserName),
            ok;       
        {none, update} ->
            em_srd:set_e164(Event),
            em_hss:delete({enum, CurrentPhone, CurrentPubId}, State),
            em_hss:delete({teluri, UserName, CurrentPhone}, State),
            em_hss:create({teluri, Event#event{pubid=CurrentPubId}}, State),
            em_hss:create({enum, Event#event{pubid=CurrentPubId}}, State),    
            ok;
        {none, create} ->
            em_srd:set_e164(Event),
            em_hss:create({teluri, Event#event{pubid=CurrentPubId}}, State),
            em_hss:create({enum, Event#event{pubid=CurrentPubId}}, State),
            ok;
        {update, none} when CurrentPhone =/= undefined ->
            em_hss:delete({enum, CurrentPhone, CurrentPubId}, State),
            em_hss:delete({teluri, UserName, CurrentPhone}, State),
            em_hss:delete({pubid, UserName, CurrentPubId}, State),
            em_hss:delete({serviceprofile, UserName, CurrentPubId}, State),
            em_srd:set_sipuri(Event), 
            em_hss:create({serviceprofile, Event}, State),
            em_hss:create({pubid, Event}, State),
            em_hss:create({teluri, Event}, State),
            em_hss:create({enum, Event}, State),
            ok;
        {update, none} ->
            em_hss:delete({pubid, UserName, CurrentPubId}, State),
            em_hss:delete({serviceprofile, UserName, CurrentPubId}, State),
            em_srd:set_sipuri(Event),
            em_hss:create({serviceprofile, Event}, State),
            em_hss:create({pubid, Event}, State),
            ok;
        {update, update} ->
            em_hss:delete({enum, CurrentPhone, CurrentPubId}, State),
            em_hss:delete({teluri, UserName, CurrentPhone}, State),
            em_hss:delete({pubid, UserName, CurrentPubId}, State),
            em_hss:delete({serviceprofile, UserName, CurrentPubId}, State),
            em_srd:set_sipuri(Event),
            em_srd:set_e164(Event),
            em_hss:create({serviceprofile, Event}, State),
            em_hss:create({pubid, Event}, State),
            em_hss:create({teluri, Event}, State),
            em_hss:create({enum, Event}, State),
            ok;
        {update, create} ->
            em_hss:delete({pubid, UserName, CurrentPubId}, State),
            em_hss:delete({serviceprofile, UserName, CurrentPubId}, State),
            em_srd:set_e164(Event),
            em_srd:set_sipuri(Event),
            em_hss:create({serviceprofile, Event}, State),
            em_hss:create({pubid, Event}, State),
            em_hss:create({teluri, Event}, State),
            em_hss:create({enum, Event}, State),
            ok;
        {update, delete} ->
            em_hss:delete({enum, CurrentPhone, CurrentPubId}, State),
            em_hss:delete({teluri, UserName, CurrentPhone}, State),
            em_srd:delete_e164(UserName),
            em_hss:delete({pubid, UserName, CurrentPubId}, State),
            em_hss:delete({serviceprofile, UserName, CurrentPubId}, State),
            em_srd:set_sipuri(Event),
            em_hss:create({serviceprofile, Event}, State),
            em_hss:create({pubid, Event}, State),
            ok;
        {delete, none} when CurrentPhone =/= undefined ->
            em_hss:delete({enum, CurrentPhone, CurrentPubId}, State),
            em_hss:delete({teluri, UserName, CurrentPhone}, State),
            em_hss:delete({pubid, UserName, CurrentPubId}, State),
            em_hss:delete({serviceprofile, UserName, CurrentPubId}, State),
            em_srd:set_sipuri(Event#event{pubid=UserName, sprofile=UserName}),
            em_hss:create({serviceprofile, Event#event{pubid=UserName, sprofile=UserName}}, State),
            em_hss:create({pubid, Event#event{pubid=UserName, sprofile=UserName}}, State),
            em_hss:create({teluri, Event#event{pubid=UserName, sprofile=UserName}}, State),
            em_hss:create({enum, Event#event{pubid=UserName, sprofile=UserName}}, State),
            ok;    
        {delete, none} ->
            em_hss:delete({pubid, UserName, CurrentPubId}, State),
            em_hss:delete({serviceprofile, UserName, CurrentPubId}, State),
            em_srd:set_sipuri(Event#event{pubid=UserName, sprofile=UserName}),
            em_hss:create({serviceprofile, Event#event{pubid=UserName, sprofile=UserName}}, State),
            em_hss:create({pubid, Event#event{pubid=UserName, sprofile=UserName}}, State),
            ok;
        {delete, create} ->
            em_hss:delete({pubid, UserName, CurrentPubId}, State),
            em_hss:delete({serviceprofile, UserName, CurrentPubId}, State),
            em_srd:set_sipuri(Event#event{pubid=UserName, sprofile=UserName}),
            em_hss:create({serviceprofile, Event#event{pubid=UserName, sprofile=UserName}}, State),
            em_hss:create({pubid, Event#event{pubid=UserName, sprofile=UserName}}, State),
            em_srd:set_e164(Event#event{pubid=UserName, sprofile=UserName}),
            em_hss:create({teluri, Event#event{pubid=UserName, sprofile=UserName}}, State),
            em_hss:create({enum, Event#event{pubid=UserName, sprofile=UserName}}, State),
            ok;
        {delete, update} ->
            em_hss:delete({enum, CurrentPhone, CurrentPubId}, State),
            em_hss:delete({teluri, UserName, CurrentPhone}, State),
            em_hss:delete({pubid, UserName, CurrentPubId}, State),
            em_hss:delete({serviceprofile, UserName, CurrentPubId}, State),
            em_srd:set_sipuri(Event#event{pubid=UserName, sprofile=UserName}),
            em_srd:set_e164(Event#event{pubid=UserName, sprofile=UserName}),
            em_hss:create({serviceprofile, Event#event{pubid=UserName, sprofile=UserName}}, State),
            em_hss:create({pubid, Event#event{pubid=UserName, sprofile=UserName}}, State),
            em_hss:create({teluri, Event#event{pubid=UserName, sprofile=UserName}}, State),
            em_hss:create({enum, Event#event{pubid=UserName, sprofile=UserName}}, State),
            ok;
        {delete, delete} ->
            em_hss:delete({enum, CurrentPhone, CurrentPubId}, State),
            em_hss:delete({teluri, UserName, CurrentPhone}, State),
            em_srd:delete_e164(UserName),
            em_hss:delete({pubid, UserName, CurrentPubId}, State),
            em_hss:delete({serviceprofile, UserName, CurrentPubId}, State),
            em_srd:set_sipuri(Event#event{pubid=UserName, sprofile=UserName}),
            em_hss:create({serviceprofile, Event#event{pubid=UserName, sprofile=UserName}}, State),
            em_hss:create({pubid, Event#event{pubid=UserName, sprofile=UserName}}, State),
            ok;         
        {ActionPubId, ActionPhone} ->
            ?ERROR_MSG("Unknown planned changes: ~p", [{ActionPubId, ActionPhone}])
            
    end;
    
modify(#event{type='trunk'}) ->
    ?INFO_MSG("THIS IS A TRUNK MODIFY ~n", []).
    
    %Delete
    %Add
    %Modify

%%%===================================================================
%%% Internal functions
%%%===================================================================
plan_change(#event{user=UserName, pubid=PubId, phone=Phone}) ->
    CurrentPubId = em_srd:get_sipuri(UserName),
    CurrentPhone = em_srd:get_e164(UserName),
    PubIdAction = plan_change(UserName, CurrentPubId, PubId),
    PhoneAction = plan_change(CurrentPhone, Phone),
    {PubIdAction, PhoneAction}.
 
plan_change(_, undefined) ->
    none;
plan_change(undefined, nil) ->
    none; 
plan_change(undefined, _X) ->
    create;        
plan_change(X, X) ->
    none;
plan_change(_X, nil) ->
    delete;    
plan_change(_X, _Y) ->
    update.

plan_change(_, _, undefined) ->
    none;
plan_change(_, _X, _X) ->
    none;
plan_change(X, X, nil) ->
    none;
plan_change(_X, _Y, nil) ->
    delete;
plan_change(_, _X, _Y) ->
    update.
    