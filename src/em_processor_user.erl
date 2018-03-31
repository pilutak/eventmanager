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

-export([create_user/1, delete_user/1, modify_user/1, set_password/2, set_phonecontext/3]).
-include("../include/em.hrl").

%%%===================================================================
%%% API
%%%===================================================================
create_user(IMSAssociation) ->
    ?INFO_MSG("Creating user: ~p", [maps:get(user, IMSAssociation)]),
    ok = em_srd:create_user(IMSAssociation),
    ok = em_ema_server:create_hss_subscriber(IMSAssociation),
    ok = em_ema_server:create_hss_serviceprofile(IMSAssociation),
    ok = em_ema_server:create_hss_pubid(IMSAssociation),
    
    case maps:get(phone, IMSAssociation) of
        'NODATA' -> ok;
        nil -> ok;
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
    CurrentUserType = em_srd:get_type(IMSAssociation),
    User = maps:get(user, IMSAssociation),
    Group = em_srd:get_group(User),

    case CurrentUserType of
        "trunk" -> 
            ?INFO_MSG("Changing trunk user type to user: ~n", []),
            delete_user(IMSAssociation),
            IMSAssociation5 = maps:put(group, Group, IMSAssociation),
            IMSAssociation6 = maps:update(pubid, User, IMSAssociation5),
            IMSAssociation7 = maps:update(sprofile, User, IMSAssociation6),
            IMSAssociation8 = maps:put(pass, randchar(14), IMSAssociation7),
            create_user(IMSAssociation8),
            modify(IMSAssociation8);
            
        "user" -> ?INFO_MSG("Already type user: ~n", []),
                modify(IMSAssociation)
    end.

modify(IMSAssociation) ->
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

        % special workaround for users where device is set to "none"
        {update, ignore} when CurrentPhone /= "NODATA" andalso PubId == undefined -> 
                            ?INFO_MSG("Deleting pubid: ~p, then set to new pubid: ~p, keeping phone",[CurrentPubId, maps:get(user, IMSAssociation)]),
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
                
        {update, ignore} when CurrentPhone /= "NODATA" -> 
                            ?INFO_MSG("Deleting pubid: ~p, then set to new pubid: ~p, keeping phone",[CurrentPubId, maps:get(pubid, IMSAssociation)]),
                            ok = em_ema_server:delete_enum(CurrentPhone),
                            ok = em_ema_server:delete_hss_teluri(AssId, CurrentPhone),
                            ok = em_ema_server:delete_hss_pubid(AssId, CurrentPubId),
                            ok = em_ema_server:delete_hss_serviceprofile(AssId, CurrentPubId),
                            ok = em_srd:set_sipuri(IMSAssociation),
                            ok = em_ema_server:create_hss_serviceprofile(IMSAssociation),
                            ok = em_ema_server:create_hss_pubid(IMSAssociation),
                            ok = em_ema_server:create_hss_teluri(IMSAssociation),
                            ok = em_ema_server:create_enum(IMSAssociation);

        % special workaround for users where device is set to "none"
        {update, ignore} when CurrentPhone == "NODATA" andalso PubId == undefined -> 
                            ?INFO_MSG("Deleting pubid: ~p, then set to new pubid: ~p",[CurrentPubId, maps:get(user, IMSAssociation)]),
                            ok = em_ema_server:delete_hss_pubid(AssId, CurrentPubId),
                            ok = em_ema_server:delete_hss_serviceprofile(AssId, CurrentPubId),
                            IMSAssociation1 = maps:update(pubid, User, IMSAssociation),
                            ok = em_srd:set_sipuri(IMSAssociation1),
                            ok = em_ema_server:create_hss_serviceprofile(IMSAssociation1),
                            ok = em_ema_server:create_hss_pubid(IMSAssociation1);

        {update, ignore} when CurrentPhone == "NODATA" -> 
                            ?INFO_MSG("Deleting pubid: ~p, then set to new pubid: ~p",[CurrentPubId, maps:get(pubid, IMSAssociation)]),
                            ok = em_ema_server:delete_hss_pubid(AssId, CurrentPubId),
                            ok = em_ema_server:delete_hss_serviceprofile(AssId, CurrentPubId),
                            ok = em_srd:set_sipuri(IMSAssociation),
                            ok = em_ema_server:create_hss_serviceprofile(IMSAssociation),
                            ok = em_ema_server:create_hss_pubid(IMSAssociation);
        % fix for users
        {update, delete} when PubId == undefined ->
                            ?INFO_MSG("Deleting pubid: ~p, then set to new pubid: ~p, deleting phone: ~p", [CurrentPubId, PubId, CurrentPhone]),
                            ok = em_srd:delete_e164(User),
                            ok = em_ema_server:delete_enum(CurrentPhone),
                            ok = em_ema_server:delete_hss_teluri(AssId, CurrentPhone),
                            ok = em_ema_server:delete_hss_pubid(AssId, CurrentPubId),
                            ok = em_ema_server:delete_hss_serviceprofile(AssId, CurrentPubId),
                            IMSAssociation1 = maps:update(pubid, User, IMSAssociation),
                            ok = em_srd:set_sipuri(IMSAssociation1),
                            ok = em_ema_server:create_hss_serviceprofile(IMSAssociation1),
                            ok = em_ema_server:create_hss_pubid(IMSAssociation1);        
        % fix for users
        {update, delete} when PubId /= undefined ->
                            ?INFO_MSG("Deleting pubid: ~p, then set to new pubid: ~p, deleting phone: ~p", [CurrentPubId, PubId, CurrentPhone]),
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

        % fix for users
        {update, update} when PubId == undefined ->
                            ?INFO_MSG("Deleting pubid: ~p, then set to new pubid: ~p, deleting phone: ~p, creating phone: ~p", [CurrentPubId, PubId, CurrentPhone, Phone]),
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
        % fix for users
        {update, update} when PubId /= undefined -> 
                            ?INFO_MSG("Deleting pubid: ~p, then set to new pubid: ~p, deleting phone: ~p, creating phone: ~p", [CurrentPubId, PubId, CurrentPhone, Phone]),
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
    
set_password(UserName, Pass) ->
    em_ema_server:update_hss_pass(UserName, Pass),
    em_srd:set_pass(UserName, Pass).
    
set_phonecontext(_UserName, PhoneContext, PhoneContext) ->
    ignore;
set_phonecontext(_UserName, undefined, "NODATA") ->
    ignore;
    
%set_phonecontext(UserName, undefined, _CurrentPhoneContext) ->
%    State=#state{session=em_ema_session:open()},
%    em_hss:update({phonecontext, UserName, "tg.gl"}, State),
%    em_srd:set_phonecontext(UserName, "tg.gl"),
%    em_ema_session:close(State);
            
set_phonecontext(UserName, PhoneContext, _CurrentPhoneContext) ->
    em_ema_server:update_hss_phonecontext(UserName, PhoneContext),
    em_srd:set_phonecontext(UserName, PhoneContext).


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
plan_phone_change(undefined, _Y) ->
    delete;
plan_phone_change(X, X) ->
    ignore;
plan_phone_change(_X, "NODATA") ->
    create;
plan_phone_change(_X, _Y) ->
    update.    

% We use this to create a temporarily SIP password. The password is later
% overwritten by an seperate event (not for virtual users, the password remains).
randchar(N) ->
   randchar(N, []).
   
randchar(0, Acc) ->
   Acc;
randchar(N, Acc) ->
   randchar(N - 1, [rand:uniform(26) + 96 | Acc]). 
