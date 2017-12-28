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

-module(em_processor_service).

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
            nil ->
                em_hss:delete({subscriber, UserName}, State),
                {ok, _} = em_srd:delete_user(UserName);
           
            _ ->
                em_hss:delete({enum, CurrentPhone, CurrentPubId}, State),
                em_hss:delete({subscriber, UserName}, State),
                {ok, _} = em_srd:delete_user(UserName)
        end,
    em_ema_session:close(State).   


modify(#event{pubid=undefined, phone=undefined}) ->
    ?INFO_MSG("Ignoring (all undefined): ~n", []); 

modify(#event{current_pubid=X1, pubid=X1, phone=Y1, current_phone=Y1}) ->
    ?INFO_MSG("Ignoring (default pubid already set): ~n", []); 

modify(#event{current_pubid=X1, pubid=X1, phone=nil, current_phone=undefined}) ->
    ?INFO_MSG("Ignoring (phone nil, current phone undefined): ~n", []); 
    
modify(#event{user=Z1, current_pubid=Z1, pubid=nil, phone=Y2, current_phone=Y2}) ->
    ?INFO_MSG("Ignoring (pubid default, no phone change): ~n", []); 

% State: PublicId is set, no phone exist.
% CommPilot: PublicId = empty, Phone = empty  
modify(Event=#event{user=Z1, current_pubid=X1, pubid=nil, phone=nil, current_phone=undefined}) ->
    ?INFO_MSG("Updating pubid to default PubId: ~n", []),
    State=#state{session=em_ema_session:open()}, 
    em_hss:delete({pubid, Z1, X1}, State),
    em_hss:delete({serviceprofile, Z1, X1}, State),
    em_srd:set_sipuri(Event#event{pubid=Z1, sprofile=Z1}),
    em_hss:create({serviceprofile, Event#event{pubid=Z1, sprofile=Z1}}, State),
    em_hss:create({pubid, Event#event{pubid=Z1, sprofile=Z1}}, State),
    em_ema_session:close(State);  

% State: PublicId is set, no phone exist.
% CommPilot: PublicId = empty, Phone = a number is selected 
modify(Event=#event{current_pubid=X1, pubid=nil, phone=Y1, current_phone=undefined}) ->
    ?INFO_MSG("Creating phone: ~p", [Y1]),
    State=#state{session=em_ema_session:open()},
    em_srd:set_e164(Event),
    em_hss:create({teluri, Event#event{pubid=X1}}, State),
    em_hss:create({enum, Event#event{pubid=X1}}, State),
    em_ema_session:close(State); 

% State: PublicId is set, no phone exist.
% CommPilot: PublicId = default pubId is set, Phone = a number is selected  
modify(Event=#event{current_pubid=X1, pubid=X1, phone=Y1, current_phone=undefined}) ->
    ?INFO_MSG("Creating phone: ~p", [Y1]),
    State=#state{session=em_ema_session:open()},
    em_srd:set_e164(Event),
    em_hss:create({teluri, Event#event{pubid=X1}}, State),
    em_hss:create({enum, Event#event{pubid=X1}}, State),
    em_ema_session:close(State); 

% State: PublicId is set, phone exist.
% CommPilot: PublicId = pubId is set, Phone = empty  
modify(#event{user=Z1, current_pubid=X1, pubid=X1, phone=nil, current_phone=Y1}) ->
    ?INFO_MSG("Deleting phone: ~n", []),
    State=#state{session=em_ema_session:open()},
    em_hss:delete({enum, Y1, X1}, State),
    em_hss:delete({teluri, Z1, Y1}, State),
    em_srd:delete_e164(Z1),
    em_ema_session:close(State);  

% State: PublicId is set, phone exist.
% CommPilot: PublicId = pubId is set, Phone = empty  
    modify(Event=#event{user=Z1, current_pubid=X1, pubid=X1, phone=Y1, current_phone=Y2}) ->
    ?INFO_MSG("Updating phone (no pubid change): ~p", [Y1]),
    State=#state{session=em_ema_session:open()},
    em_hss:delete({enum, Y2, X1}, State),
    em_hss:delete({teluri, Z1, Y2}, State),
    em_srd:set_e164(Event),
    em_hss:create({teluri, Event}, State),
    em_hss:create({enum, Event}, State),
    em_ema_session:close(State);  

% State: PublicId is set to default, phone exist.
% CommPilot: PublicId = empty, Phone = empty      
modify(#event{user=Z1, current_pubid=Z1, pubid=nil, phone=nil, current_phone=Y2}) ->
    ?INFO_MSG("Deleting phone (no pubid change, already default): ~n", []),
    State=#state{session=em_ema_session:open()},
    em_hss:delete({enum, Y2, Z1}, State),
    em_hss:delete({teluri, Z1, Y2}, State),
    em_srd:delete_e164(Z1),
    em_ema_session:close(State);  

% State: PublicId is set to default, phone exist.
% CommPilot: PublicId = empty, Phone = is selected (new number)      
modify(Event=#event{user=Z1, current_pubid=Z1, pubid=nil, phone=_Y1, current_phone=Y2}) ->
    ?INFO_MSG("Updating phone (no pubid change, default): ~n", []),
    State=#state{session=em_ema_session:open()},
    em_hss:delete({enum, Y2, Z1}, State),
    em_hss:delete({teluri, Z1, Y2}, State),
    em_srd:set_e164(Event),
    em_hss:create({teluri, Event#event{pubid=Z1, sprofile=Z1}}, State),
    em_hss:create({enum, Event#event{pubid=Z1, sprofile=Z1}}, State),
    em_ema_session:close(State);  
 
% State: PublicId is set, phone exist.
% CommPilot: PublicId = empty, Phone = is selected (current number)      
modify(Event=#event{user=Z1, current_pubid=X1, pubid=nil, phone=Y1, current_phone=Y1}) ->
    ?INFO_MSG("deleting pubid (no change to phone): ~p", [X1]),
    State=#state{session=em_ema_session:open()},
    em_hss:delete({enum, Y1, X1}, State),
    em_hss:delete({teluri, Z1, Y1}, State),
    em_hss:delete({pubid, Z1, X1}, State),
    em_hss:delete({serviceprofile, Z1, X1}, State),
    em_srd:set_sipuri(Event#event{pubid=Z1, sprofile=Z1}),
    em_hss:create({serviceprofile, Event#event{pubid=Z1, sprofile=Z1}}, State),
    em_hss:create({pubid, Event#event{pubid=Z1, sprofile=Z1}}, State),
    em_hss:create({teluri, Event#event{pubid=Y1, sprofile=Z1}}, State),
    em_hss:create({enum, Event#event{pubid=Y1, sprofile=Z1}}, State),
    em_ema_session:close(State);

% State: PublicId is set, no phone exist.
% CommPilot: PublicId = empty, Phone = is empty     
modify(Event=#event{user=Z1, current_pubid=X1, phone=nil, current_phone=undefined}) ->
    ?INFO_MSG("Updating pubid, no change to phone, since undefined): ~n", []),    
    State=#state{session=em_ema_session:open()},
    em_hss:delete({pubid, Z1, X1}, State),
    em_hss:delete({serviceprofile, Z1, X1}, State),
    em_srd:set_sipuri(Event),
    em_hss:create({serviceprofile, Event}, State),
    em_hss:create({pubid, Event}, State),
    em_ema_session:close(State);  

% State: PublicId is set, phone exist.
% CommPilot: PublicId = is set, Phone = is selectes (current number)      
modify(Event=#event{user=Z1, current_pubid=X1, phone=Y1, current_phone=Y1}) ->
    ?INFO_MSG("Updating pubid, no change to phone): ~n", []),
    State=#state{session=em_ema_session:open()},
    em_hss:delete({enum, Y1, X1}, State),
    em_hss:delete({teluri, Z1, Y1}, State),
    em_hss:delete({pubid, Z1, X1}, State),
    em_hss:delete({serviceprofile, Z1, X1}, State),
    em_srd:set_sipuri(Event),
    em_hss:create({serviceprofile, Event}, State),
    em_hss:create({pubid, Event}, State),
    em_hss:create({teluri, Event}, State),
    em_hss:create({enum, Event}, State),
    em_ema_session:close(State); 

% State: PublicId is set to default, phone exist.
% CommPilot: PublicId = is empty, Phone = is empty      
modify(#event{user=Z1, current_pubid=Z1, pubid=nil, phone=nil, current_phone=Y1}) ->
    ?INFO_MSG("deleting phone (pubid is nil and default): ~n", []),
    State=#state{session=em_ema_session:open()},
    em_hss:delete({enum, Y1, Z1}, State),
    em_hss:delete({teluri, Z1, Y1}, State),
    em_srd:delete_e164(Z1),
    em_ema_session:close(State);  

% State: PublicId is set to non default, phone exist.
% CommPilot: PublicId = is empty, Phone = is empty       
modify(Event=#event{user=Z1, current_pubid=X1, pubid=nil, phone=nil, current_phone=Y1}) ->
    ?INFO_MSG("deleting pubid and phone: ~p", [X1]),    
    State=#state{session=em_ema_session:open()},
    em_hss:delete({enum, Y1, X1}, State),
    em_hss:delete({teluri, Z1, Y1}, State),
    em_srd:delete_e164(Z1),
    em_hss:delete({pubid, Z1, X1}, State),
    em_hss:delete({serviceprofile, Z1, X1}, State),
    em_srd:set_sipuri(Event#event{pubid=Z1, sprofile=Z1}),
    em_hss:create({serviceprofile, Event#event{pubid=Z1, sprofile=Z1}}, State),
    em_hss:create({pubid, Event#event{pubid=Z1, sprofile=Z1}}, State),
    em_ema_session:close(State);     

% State: PublicId is set , phone exist.
% CommPilot: PublicId = is any, Phone = any      
modify(Event=#event{user=Z1, current_pubid=X1, current_phone=Y2}) ->
    ?INFO_MSG("Updating pubId and phone: ~p", [X1]),    
    State=#state{session=em_ema_session:open()},
    em_hss:delete({enum, Y2, X1}, State),
    em_hss:delete({teluri, Z1, Y2}, State),
    em_hss:delete({pubid, Z1, X1}, State),
    em_hss:delete({serviceprofile, Z1, X1}, State),
    em_srd:set_sipuri(Event),
    em_srd:set_e164(Event),
    em_hss:create({serviceprofile, Event}, State),
    em_hss:create({pubid, Event}, State),
    em_hss:create({teluri, Event}, State),
    em_hss:create({enum, Event}, State),
    em_ema_session:close(State);

modify(_) ->
    ?ERROR_MSG("Invalid Event record state: ~n", []).     
    
    
    
