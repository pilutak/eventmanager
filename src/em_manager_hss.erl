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

-module(em_manager_hss).

-export([create_user/1, delete_user/1, modify_user/1, modify_trunk_user/1, set_password/1, set_phonecontext/1]).
-include("../include/em.hrl").

%%%===================================================================
%%% API
%%%===================================================================
create_user(Event) ->
    ?INFO_MSG("Creating user: ~p~n", [maps:get(user, Event)]),
    create_ims_association(Event),
    ?INFO_MSG("Created user: ~p~n", [maps:get(user, Event)]).
    
delete_user(Event) ->
    ?INFO_MSG("Deleting user: ~p~n", [maps:get(user, Event)]),
    delete_ims_association(Event),
    ?INFO_MSG("Deleted user: ~p~n", [maps:get(user, Event)]).
    
        
modify_user(Event) ->
    ?INFO_MSG("Modifying user: ~p~n", [maps:get(user, Event)]),    
    CurrentUserType = em_srd:get_type(Event),
    User = maps:get(user, Event),
    Group = em_srd:get_group(User),

    case CurrentUserType of
        "trunk" -> 
            ?INFO_MSG("Modify trunk user type to user: ~n", []),
            delete_user(Event),
            Event5 = maps:put(group, Group, Event),
            Event6 = maps:update(pubid, User, Event5),
            Event7 = maps:update(sprofile, User, Event6),
            Event8 = maps:put(pass, em_utils:randchar(14), Event7),
            create_user(Event8),
            Plan = make_plan(Event8),
            execute_plan(Plan, Event8);  
            
        "pilot" -> 
            ?INFO_MSG("Modify pilot user type to user: ~n", []),
            delete_user(Event),
            Event5 = maps:put(group, Group, Event),
            Event6 = maps:update(pubid, User, Event5),
            Event7 = maps:update(sprofile, User, Event6),
            Event8 = maps:put(pass, em_utils:randchar(14), Event7),
            create_user(Event8),
            Plan = make_plan(Event8),
            execute_plan(Plan, Event8);
            
        "user" ->    ?INFO_MSG("Modify type is user: ~n", []),
                     Plan1 = make_plan(Event),
                     execute_plan(Plan1, Event);
        "virtual" -> ?INFO_MSG("Modify type is virtual: ~n", []), 
                     Plan1 = make_plan(Event),
                     execute_plan(Plan1, Event)
    end.
    
modify_trunk_user(Event) ->
    ?INFO_MSG("Modifying trunk user: ~p~n", [maps:get(user, Event)]),    
    CurrentUserType = em_srd:get_type(Event),
    User = maps:get(user, Event),
    Group = em_srd:get_group(User),
    
    case CurrentUserType of
        "user" -> 
            ?INFO_MSG("Changing user type to trunk: ~p~n", [User]),
            delete_user(Event),
            Event1 = maps:put(group, Group, Event),
            create_user(Event1),
            Plan1 = make_plan(Event1),
            execute_plan(Plan1, Event1);
        "trunk" -> ?INFO_MSG("Already user type trunk: ~p~n", [User]),
                Plan2 = make_plan(Event),
                execute_plan(Plan2, Event);
        "pilot" -> ?INFO_MSG("Already user type pilot: ~p~n", [User]),
                Event5 = maps:put(type, 'pilot', Event),
                Event6 = maps:update(csprofile, ?SIPTRUNKPROFILE_PILOT, Event5),
                Plan2 = make_plan(Event6),
                execute_plan(Plan2, Event6)
        
    end.   
    
set_password(#{ user := User} = Event) ->
    ?INFO_MSG("Updating SIP password for user: ~p~n", [User]),
    CAI3G = em_cai3g_envelope:set_ims_pass(Event),
    ok = em_ema:request(CAI3G).
    
set_phonecontext(#{ user := User, phonecontext := Context} = Event ) ->
    CContext = em_srd:get_phonecontext(User),
    PubId = em_srd:get_sipuri(Event),
     ?INFO_MSG("PUBID IS: ~p~n", [PubId]),
     ?INFO_MSG("CONTEXT IS: ~p~n", [Context]),
     ?INFO_MSG("CCOntext IS: ~p~n", [CContext]),
    case Context == CContext of
        true -> ok;
        false -> Event1 = maps:put(pubid, PubId, Event),
            em_srd:set_phonecontext(User, Context),
            CAI3G = em_cai3g_envelope:set_ims_phonecontext(Event1),
            ok = em_ema:request(CAI3G)
    end.
    
%%%===================================================================
%%% Internal functions
%%%===================================================================
make_plan(#{ user := User, pubid := PubId, phone := Phone } = Event) ->
    ?INFO_MSG("Making plan for: ~p~n", [User]), 
    CPubId = em_srd:get_sipuri(Event),
    CPhone = em_srd:get_e164(Event),

    PubIdAction = plan_pubid_change(PubId, CPubId, User),
    PhoneAction = plan_phone_change(Phone, CPhone),
    
    {PubIdAction, PhoneAction}.

execute_plan(Plan, Event) ->
    ?INFO_MSG("Executing plan: ~p~n", [Plan]), 
    case Plan of        
        {ignore, ignore} -> ok;
        {ignore, delete} -> delete_phone(Event);
        {ignore, create} -> create_phone(Event);
        {ignore, update} -> delete_phone(Event),
                            create_phone(Event);                            
        {delete, ignore} -> delete_impu_sip(Event);
        {delete, delete} -> delete_phone(Event),
                            delete_impu_sip(Event);
        {delete, create} -> delete_impu_sip(Event),
                            create_phone(Event);
        {delete, update} -> delete_phone(Event),                            
                            delete_impu_sip(Event),
                            create_phone(Event);
        {update, ignore} -> update_impu_sip(Event);
        {update, delete} -> delete_phone(Event),                            
                            update_impu_sip(Event);
        {update, create} -> update_impu_sip(Event),
                            create_phone(Event);
        {update, update} -> delete_phone(Event),                            
                            update_impu_sip(Event),
                            create_phone(Event);
                                                
        Err -> ?ERROR_MSG("No execution selection found for plan: ~p~n", [Err])
    end.

create_ims_association(#{ phone := "NODATA", type := Type} = Event) ->
    ok = em_srd:create_user(Event),
    case Type of
        user ->
            CAI3G = em_cai3g_envelope:add_ims_subscriber(Event),
            ok = em_ema:request(CAI3G);
        virtual ->
            CAI3G = em_cai3g_envelope:add_ims_virtual_subscriber( Event),
            ok = em_ema:request(CAI3G);
        trunk ->
            CAI3G = em_cai3g_envelope:add_ims_virtual_subscriber( Event),
            ok = em_ema:request(CAI3G); 
        pilot ->
            CAI3G = em_cai3g_envelope:add_ims_subscriber(Event),
            ok = em_ema:request(CAI3G)
    end;
create_ims_association(#{ phone := nil, type := user } = Event) ->
    ok = em_srd:create_user(Event),
    CAI3G = em_cai3g_envelope:add_ims_subscriber(Event),
    ok = em_ema:request(CAI3G);
create_ims_association(#{ phone := nil, type := trunk } = Event) ->
    ok = em_srd:create_user(Event),
    CAI3G = em_cai3g_envelope:add_ims_virtual_subscriber(Event),
    ok = em_ema:request(CAI3G); 
create_ims_association(#{ phone := nil, type := pilot } = Event) ->
    ok = em_srd:create_user(Event),
    CAI3G = em_cai3g_envelope:add_ims_subscriber(Event),
    ok = em_ema:request(CAI3G);     
create_ims_association(#{ type := Type } = Event) ->
    ok = em_srd:create_user(Event),
    case Type of
        user ->
            CAI3G = em_cai3g_envelope:add_ims_subscriber(Event),
            ok = em_ema:request(CAI3G); 
        virtual ->
            CAI3G = em_cai3g_envelope:add_ims_virtual_subscriber(Event),
            ok = em_ema:request(CAI3G); 

        trunk ->
            CAI3G = em_cai3g_envelope:add_ims_virtual_subscriber(Event),
            ok = em_ema:request(CAI3G); 

        pilot ->
            CAI3G = em_cai3g_envelope:add_ims_subscriber(Event),
            ok = em_ema:request(CAI3G) 
    end,
    ok = em_srd:set_e164(Event),
    CAI3G1 = em_cai3g_envelope:add_ims_teluri(Event),
    ok = em_ema:request(CAI3G1), 
    
    CAI3G2 = em_cai3g_envelope:add_ims_enum(Event),
    ok = em_ema:request(CAI3G2).
    
% copy from service
delete_phone(#{ user := User, association := AId } = Event) ->
    CPhone = em_srd:get_e164(Event),
    ok = em_srd:delete_e164(User),
    CAI3G = em_cai3g_envelope:delete_ims_enum(CPhone),
    ok = em_ema:request(CAI3G), 
    CAI3G1 = em_cai3g_envelope:delete_ims_teluri(AId, CPhone),
    ok = em_ema:request(CAI3G1).


% special handling of users
create_phone(#{ pubid := nil, user := User } = Event) ->
    Event1 = maps:update(pubid, User, Event),
    ok = em_srd:set_e164(Event1),

    CAI3G1 = em_cai3g_envelope:add_ims_teluri(Event1),
    ok = em_ema:request(CAI3G1), 
    
    CAI3G2 = em_cai3g_envelope:add_ims_enum(Event1),
    ok = em_ema:request(CAI3G2);

create_phone(#{ pubid := undefined, user := User } = Event) ->
    Event1 = maps:update(pubid, User, Event),
    ok = em_srd:set_e164(Event1),
    CAI3G1 = em_cai3g_envelope:add_ims_teluri(Event1),
    ok = em_ema:request(CAI3G1), 
    
    CAI3G2 = em_cai3g_envelope:add_ims_enum(Event1),
    ok = em_ema:request(CAI3G2);

create_phone(Event) ->
    ok = em_srd:set_e164(Event),
    CAI3G1 = em_cai3g_envelope:add_ims_teluri(Event),
    ok = em_ema:request(CAI3G1), 
    
    CAI3G2 = em_cai3g_envelope:add_ims_enum(Event),
    ok = em_ema:request(CAI3G2).

delete_impu_sip(#{ user := User, association := AId } = Event) ->
    CPubId = em_srd:get_sipuri(Event),
    CPhone = em_srd:get_e164(Event),
    
    case CPhone of
        "NODATA" -> ok;        
        _ ->
            delete_phone(Event)
    end,

    CAI3G1 = em_cai3g_envelope:delete_ims_pubid(AId, CPubId),
    ok = em_ema:request(CAI3G1),
        
    CAI3G2 = em_cai3g_envelope:delete_ims_serviceprofile(AId, CPubId),
    ok = em_ema:request(CAI3G2),
    
    Event1 = maps:update(pubid, User, Event),
    ok = em_srd:set_sipuri(Event1),

    CAI3G3 = em_cai3g_envelope:add_ims_serviceprofile(Event1),
    ok = em_ema:request(CAI3G3),

    CAI3G4 = em_cai3g_envelope:add_ims_pubid(Event1),
    ok = em_ema:request(CAI3G4),
    
    case CPhone of
        "NODATA" -> ok;        
        _ ->
            ok = create_phone(Event1)
    end.

% special handling of users
update_impu_sip(#{ user := User, association := AId, pubid := undefined } = Event) ->
    CPubId = em_srd:get_sipuri(Event),
    CPhone = em_srd:get_e164(Event),
    
    case CPhone of
        "NODATA" -> ok;        
        _ ->
            delete_phone(Event)
    end,

    CAI3G1 = em_cai3g_envelope:delete_ims_pubid(AId, CPubId),
    ok = em_ema:request(CAI3G1),
        
    CAI3G2 = em_cai3g_envelope:delete_ims_serviceprofile(AId, CPubId),
    ok = em_ema:request(CAI3G2),
    
    Event1 = maps:update(pubid, User, Event),
    ok = em_srd:set_sipuri(Event1),
    
    CAI3G3 = em_cai3g_envelope:add_ims_serviceprofile(Event1),
    ok = em_ema:request(CAI3G3),

    CAI3G4 = em_cai3g_envelope:add_ims_pubid(Event1),
    ok = em_ema:request(CAI3G4),
    
    case CPhone of
        "NODATA" -> ok;        
        _ ->
            Event2 = maps:update(phone, CPhone, Event1),
            ok = create_phone(Event2)
    end;

update_impu_sip(#{ association := AId } = Event) ->
    CPubId = em_srd:get_sipuri(Event),
    CPhone = em_srd:get_e164(Event),
    
    case CPhone of
        "NODATA" -> ok;        
        _ ->
            delete_phone(Event)
    end,

    CAI3G1 = em_cai3g_envelope:delete_ims_pubid(AId, CPubId),
    ok = em_ema:request(CAI3G1),
        
    CAI3G2 = em_cai3g_envelope:delete_ims_serviceprofile(AId, CPubId),
    ok = em_ema:request(CAI3G2),
    
    ok = em_srd:set_sipuri(Event),
    CAI3G3 = em_cai3g_envelope:add_ims_serviceprofile(Event),
    ok = em_ema:request(CAI3G3),

    CAI3G4 = em_cai3g_envelope:add_ims_pubid(Event),
    ok = em_ema:request(CAI3G4),
    
    case CPhone of
        "NODATA" -> ok;        
        _ ->
            Event1 = maps:update(phone, CPhone, Event),
            ok = create_phone(Event1)
    end.

delete_ims_association(Event) ->
    CPhone = em_srd:get_e164(Event),    
    case CPhone of
        "NODATA" -> ok;        
        _ ->
            CAI3G = em_cai3g_envelope:delete_ims_enum(CPhone),
            ok = em_ema:request(CAI3G)
            
    end,
    ok = em_srd:delete_user(Event),
    CAI3G1 = em_cai3g_envelope:delete_ims_subscriber(Event),
    em_ema:request(CAI3G1).

plan_pubid_change(nil, _X, _X) ->
    ignore;
plan_pubid_change(nil, _X, _Y) ->
    delete;
plan_pubid_change(X, X, _) ->
    ignore;
plan_pubid_change(_X, _Y, _) ->
    update.

% special handling of users
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
   
   
%%%===================================================================
%%% Unit Tests
%%%===================================================================
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

plan_pubid_change_test_() ->
    [?_assert(plan_pubid_change(nil, "user@tz4.com", "user@tz4.com") =:= ignore),
     ?_assert(plan_pubid_change(nil, "hello@tz4.com", "user@tz4.com") =:= delete),
     ?_assert(plan_pubid_change("hello@tz4.com", "hello@tz4.com", "user@tz4.com") =:= ignore),
     ?_assert(plan_pubid_change("hello@tz4.com", "world@tz4.com", "user@tz4.com") =:= update)
    ].
    
 plan_phone_change_test_() ->
    [?_assert(plan_phone_change(nil, "NODATA") =:= ignore),
     ?_assert(plan_phone_change(nil, "299123456") =:= delete),
     ?_assert(plan_phone_change(undefined, "299123456") =:= delete),     
     ?_assert(plan_phone_change("299123456", "299123456") =:= ignore),
     ?_assert(plan_phone_change("299123456", "NODATA") =:= create),
     ?_assert(plan_phone_change("299123456", "111111") =:= update)
    ].
-endif.       
   
