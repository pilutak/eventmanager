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

-export([create_user/1, delete_user/1, modify_user/1]).
-include("../include/em.hrl").

%%%===================================================================
%%% API
%%%===================================================================
create_user(IMSAssociation) ->
    ?INFO_MSG("Creating user: ~p", [maps:get(user, IMSAssociation)]),
    ok = em_srd:create_user(IMSAssociation),
    ok = em_ema_server:create_hss_virtual_subscriber(IMSAssociation),
    ok = em_ema_server:create_hss_serviceprofile(IMSAssociation),
    ok = em_ema_server:create_hss_pubid(IMSAssociation),
    
    case maps:get(phone, IMSAssociation) of
        'NODATA' -> ok;
        _ -> ok = em_srd:set_e164(IMSAssociation),
             ok = em_ema_server:create_hss_teluri(IMSAssociation),
             ok = em_ema_server:create_enum(IMSAssociation)
    end.
    

delete_user(IMSAssociation) ->
    ?INFO_MSG("Deleting user: ~p", [maps:get(user, IMSAssociation)]),
    CurrentPhone = em_srd:get_e164(IMSAssociation),
    
    case CurrentPhone of
        "NODATA" -> ok;           
        _ ->
            ok = em_ema_server:delete_enum(CurrentPhone)
    end,
    
    ok = em_srd:delete_user(IMSAssociation),
    ok = em_ema_server:delete_hss_subscriber(IMSAssociation).
    
modify_user(IMSAssociation) ->
    ?INFO_MSG("Modifying user: ~p", [maps:get(user, IMSAssociation)]),    
    CurrentPubId = em_srd:get_sipuri(IMSAssociation),
    CurrentPhone = em_srd:get_e164(IMSAssociation),

    User = maps:get(user, IMSAssociation),
    AssId = maps:get(association, IMSAssociation),
    PubId = maps:get(pubid, IMSAssociation),
    Phone = maps:get(phone, IMSAssociation),

    PubIdAction = plan_pubid_change(PubId, CurrentPubId, User),
    PhoneAction = plan_phone_change(Phone, CurrentPhone),

    case {PubIdAction, PhoneAction} of
        {ignore, ignore} -> ?INFO_MSG("No planned change for user: ~p", [User]);
        {ignore, delete} -> ?INFO_MSG("Deleting phone: ~p from user: ~p", [CurrentPhone, User]),
                            ok = em_srd:delete_e164(User),
                            ok = em_ema_server:delete_enum(CurrentPhone),
                            ok = em_ema_server:delete_hss_teluri(AssId, CurrentPhone);
                            
        {ignore, create} when PubId /= nil -> 
                            ?INFO_MSG("Creating phone: ~p for user ~p", [Phone, User]),
                            ok = em_srd:set_e164(IMSAssociation),
                            ok = em_ema_server:create_hss_teluri(IMSAssociation),
                            ok = em_ema_server:create_enum(IMSAssociation);

        {ignore, create} when PubId == nil -> 
                            ?INFO_MSG("Creating phone: ~p for user ~p", [Phone, User]),
                            IMSAssociation1 = maps:update(pubid, User, IMSAssociation),
                            ok = em_srd:set_e164(IMSAssociation1),
                            ok = em_ema_server:create_hss_teluri(IMSAssociation1),
                            ok = em_ema_server:create_enum(IMSAssociation1);
                            
        {ignore, update} when PubId /= nil -> 
                            ?INFO_MSG("Updating phone: ~p to ~p for user: ~p", [CurrentPhone, Phone, User]),
                            ok = em_ema_server:delete_enum(CurrentPhone),
                            ok = em_ema_server:delete_hss_teluri(AssId, CurrentPhone),
                            ok = em_srd:set_e164(IMSAssociation),
                            ok = em_ema_server:create_hss_teluri(IMSAssociation),
                            ok = em_ema_server:create_enum(IMSAssociation);

        {ignore, update} when PubId == nil -> 
                            ?INFO_MSG("Updating phone: ~p to ~p for user: ~p", [CurrentPhone, Phone, User]),
                            IMSAssociation1 = maps:update(pubid, User, IMSAssociation),
                            ok = em_ema_server:delete_enum(CurrentPhone),
                            ok = em_ema_server:delete_hss_teluri(AssId, CurrentPhone),
                            ok = em_srd:set_e164(IMSAssociation1),
                            ok = em_ema_server:create_hss_teluri(IMSAssociation1),
                            ok = em_ema_server:create_enum(IMSAssociation1);
            
        {delete, ignore} when CurrentPhone /= "NODATA" -> 
                            ?INFO_MSG("Deleting pubid: ~p, then set to user: ~p", [CurrentPubId, User]),
                            ok = em_ema_server:delete_enum(CurrentPhone),
                            ok = em_ema_server:delete_hss_teluri(AssId, CurrentPhone),
                            ok = em_ema_server:delete_hss_pubid(AssId, CurrentPubId),
                            ok = em_ema_server:delete_hss_serviceprofile(AssId, CurrentPubId),
                            IMSAssociation1 = maps:update(pubid, User, IMSAssociation),
                            ok = em_srd:set_sipuri(IMSAssociation1),
                            ok = em_ema_server:create_hss_serviceprofile(IMSAssociation1),
                            ok = em_ema_server:create_hss_pubid(IMSAssociation1),
                            ok = em_ema_server:create_hss_teluri(IMSAssociation1),
                            ok = em_ema_server:create_enum(IMSAssociation1);        

        {delete, ignore} when CurrentPhone == "NODATA" -> 
                            ?INFO_MSG("Deleting pubid: ~p, then set to user: ~p", [CurrentPubId, User]),
                            ok = em_ema_server:delete_hss_pubid(AssId, CurrentPubId),
                            ok = em_ema_server:delete_hss_serviceprofile(AssId, CurrentPubId),
                            IMSAssociation1 = maps:update(pubid, User, IMSAssociation),
                            ok = em_srd:set_sipuri(IMSAssociation1),
                            ok = em_ema_server:create_hss_serviceprofile(IMSAssociation1),
                            ok = em_ema_server:create_hss_pubid(IMSAssociation1);      
        
        {delete, delete} -> ?INFO_MSG("Deleting pubid: ~p, then set to user, deleting phone: ~p", [CurrentPubId, CurrentPhone]),
                            ok = em_srd:delete_e164(User),
                            ok = em_ema_server:delete_enum(CurrentPhone),
                            ok = em_ema_server:delete_hss_teluri(AssId, CurrentPhone),
                            ok = em_ema_server:delete_hss_pubid(AssId, CurrentPubId),
                            ok = em_ema_server:delete_hss_serviceprofile(AssId, CurrentPubId),
                            IMSAssociation1 = maps:update(pubid, User, IMSAssociation),
                            ok = em_srd:set_sipuri(IMSAssociation1),
                            ok = em_ema_server:create_hss_serviceprofile(IMSAssociation1),
                            ok = em_ema_server:create_hss_pubid(IMSAssociation1);        
                
        {delete, create} -> ?INFO_MSG("Deleting pubid: ~p ,then set to user, create phone: ~p", [CurrentPubId, Phone]),
                            ok = em_ema_server:delete_hss_pubid(AssId, CurrentPubId),
                            ok = em_ema_server:delete_hss_serviceprofile(AssId, CurrentPubId),
                            IMSAssociation1 = maps:update(pubid, User, IMSAssociation),
                            ok = em_srd:set_sipuri(IMSAssociation1),
                            ok = em_ema_server:create_hss_serviceprofile(IMSAssociation1),
                            ok = em_ema_server:create_hss_pubid(IMSAssociation1),       
                            ok = em_srd:set_e164(IMSAssociation1),
                            ok = em_ema_server:create_hss_teluri(IMSAssociation1),
                            ok = em_ema_server:create_enum(IMSAssociation1);
        
        {delete, update} -> ?INFO_MSG("Deleting pubid: ~p, then set to user, deleting phone: ~p, creating phone: ~p", [CurrentPubId, CurrentPhone, Phone]),
                            ok = em_ema_server:delete_enum(CurrentPhone),
                            ok = em_ema_server:delete_hss_teluri(AssId, CurrentPhone),
                            ok = em_ema_server:delete_hss_pubid(AssId, CurrentPubId),
                            ok = em_ema_server:delete_hss_serviceprofile(AssId, CurrentPubId),
                            IMSAssociation1 = maps:update(pubid, User, IMSAssociation),
                            ok = em_srd:set_sipuri(IMSAssociation1),
                            ok = em_ema_server:create_hss_serviceprofile(IMSAssociation1),
                            ok = em_ema_server:create_hss_pubid(IMSAssociation1),        
                            ok = em_srd:set_e164(IMSAssociation1),
                            ok = em_ema_server:create_hss_teluri(IMSAssociation1),
                            ok = em_ema_server:create_enum(IMSAssociation1);
                
        {update, ignore} when CurrentPhone /= "NODATA" -> 
                            ?INFO_MSG("Deleting pubid: ~p, then set to new pubid: ~p, including phone",[CurrentPubId, maps:get(pubid, IMSAssociation)]),
                            ok = em_ema_server:delete_enum(CurrentPhone),
                            ok = em_ema_server:delete_hss_teluri(AssId, CurrentPhone),
                            ok = em_ema_server:delete_hss_pubid(AssId, CurrentPubId),
                            ok = em_ema_server:delete_hss_serviceprofile(AssId, CurrentPubId),
                            ok = em_srd:set_sipuri(IMSAssociation),
                            ok = em_ema_server:create_hss_serviceprofile(IMSAssociation),
                            ok = em_ema_server:create_hss_pubid(IMSAssociation),
                            ok = em_ema_server:create_hss_teluri(IMSAssociation),
                            ok = em_ema_server:create_enum(IMSAssociation);

        {update, ignore} when CurrentPhone == "NODATA" -> 
                            ?INFO_MSG("Deleting pubid: ~p, then set to new pubid: ~p",[CurrentPubId, maps:get(pubid, IMSAssociation)]),
                            ok = em_ema_server:delete_hss_pubid(AssId, CurrentPubId),
                            ok = em_ema_server:delete_hss_serviceprofile(AssId, CurrentPubId),
                            ok = em_srd:set_sipuri(IMSAssociation),
                            ok = em_ema_server:create_hss_serviceprofile(IMSAssociation),
                            ok = em_ema_server:create_hss_pubid(IMSAssociation);

        {update, delete} -> ?INFO_MSG("Deleting pubid: ~p, then set to new pubid: ~p, deleting phone: ~p", [CurrentPubId, PubId, CurrentPhone]),
                            ok = em_srd:delete_e164(User),
                            ok = em_ema_server:delete_enum(CurrentPhone),
                            ok = em_ema_server:delete_hss_teluri(AssId, CurrentPhone),
                            ok = em_ema_server:delete_hss_pubid(AssId, CurrentPubId),
                            ok = em_ema_server:delete_hss_serviceprofile(AssId, CurrentPubId),
                            ok = em_srd:set_sipuri(IMSAssociation),
                            ok = em_ema_server:create_hss_serviceprofile(IMSAssociation),
                            ok = em_ema_server:create_hss_pubid(IMSAssociation);        
        
        {update, create} -> ?INFO_MSG("Deleting pubid: ~p, then set to new pubid: ~p, creating phone: ~p", [CurrentPubId, PubId, Phone]),
                            ok = em_ema_server:delete_hss_pubid(AssId, CurrentPubId),
                            ok = em_ema_server:delete_hss_serviceprofile(AssId, CurrentPubId),
                            ok = em_srd:set_sipuri(IMSAssociation),
                            ok = em_ema_server:create_hss_serviceprofile(IMSAssociation),
                            ok = em_ema_server:create_hss_pubid(IMSAssociation),      
                            ok = em_srd:set_e164(IMSAssociation),
                            ok = em_ema_server:create_hss_teluri(IMSAssociation),
                            ok = em_ema_server:create_enum(IMSAssociation);
        
        {update, update} -> ?INFO_MSG("Deleting pubid: ~p, then set to new pubid: ~p, deleting phone: ~p, creating phone: ~p", [CurrentPubId, PubId, CurrentPhone, Phone]),
                            ok = em_ema_server:delete_enum(CurrentPhone),
                            ok = em_ema_server:delete_hss_teluri(AssId, CurrentPhone),
                            ok = em_ema_server:delete_hss_pubid(AssId, CurrentPubId),
                            ok = em_ema_server:delete_hss_serviceprofile(AssId, CurrentPubId),
                            ok = em_srd:set_sipuri(IMSAssociation),
                            ok = em_ema_server:create_hss_serviceprofile(IMSAssociation),
                            ok = em_ema_server:create_hss_pubid(IMSAssociation),        
                            ok = em_srd:set_e164(IMSAssociation),
                            ok = em_ema_server:create_hss_teluri(IMSAssociation),
                            ok = em_ema_server:create_enum(IMSAssociation);
        
        Err -> ?ERROR_MSG("Error: ~p", [Err])
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

plan_pubid_change(nil, _X, _X) ->
    ignore;
plan_pubid_change(nil, _X, _Y) ->
    delete;
plan_pubid_change(X, X, _) ->
    ignore;
plan_pubid_change(_X, _Y, _) ->
    update.

plan_phone_change(nil, "NODATA") ->
    ignore;
plan_phone_change(nil, _Y) ->
    delete;
plan_phone_change(X, X) ->
    ignore;
plan_phone_change(_X, "NODATA") ->
    create;
plan_phone_change(_X, _Y) ->
    update.    
