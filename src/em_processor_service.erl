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
create_user(Event) ->
    ?INFO_MSG("Creating user: ~p~n", [maps:get(user, Event)]),
    create_ims_association(Event).
    
delete_user(Event) ->
    ?INFO_MSG("Deleting user: ~p~n", [maps:get(user, Event)]),
    delete_ims_association(Event).
    
modify_user(Event) ->
    ?INFO_MSG("Modifying user: ~p~n", [maps:get(user, Event)]),  
    Plan = make_plan(Event),
    execute_plan(Plan, Event).  

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

create_ims_association(#{ phone := "NODATA", type := virtual } = Event) ->
    ok = em_srd:create_user(Event),
    ok = em_ema_server:create_hss_virtual_subscriber(Event);
create_ims_association(#{ type := virtual } = Event) ->
    ok = em_srd:create_user(Event),
    ok = em_ema_server:create_hss_virtual_subscriber(Event),
    ok = em_srd:set_e164(Event),
    ok = em_ema_server:create_hss_teluri(Event),
    ok = em_ema_server:create_enum(Event).

update_impu_sip(#{ association := AId } = Event) ->
    CPubId = em_srd:get_sipuri(Event),
    CPhone = em_srd:get_e164(Event),
    
    case CPhone of
        "NODATA" -> ok;        
        _ ->
            delete_phone(Event)
    end,

    ok = em_ema_server:delete_hss_pubid(AId, CPubId),
    ok = em_ema_server:delete_hss_serviceprofile(AId, CPubId),
    
    ok = em_srd:set_sipuri(Event),
    ok = em_ema_server:create_hss_serviceprofile(Event),
    ok = em_ema_server:create_hss_pubid(Event),      
    
    case CPhone of
        "NODATA" -> ok;        
        _ ->
            Event1 = maps:update(phone, CPhone, Event),
            ok = create_phone(Event1)
    end.


delete_impu_sip(#{ user := User, association := AId } = Event) ->
    CPubId = em_srd:get_sipuri(Event),
    CPhone = em_srd:get_e164(Event),
    
    case CPhone of
        "NODATA" -> ok;        
        _ ->
            delete_phone(Event)
    end,

    ok = em_ema_server:delete_hss_pubid(AId, CPubId),
    ok = em_ema_server:delete_hss_serviceprofile(AId, CPubId),

    Event1 = maps:update(pubid, User, Event),
    ok = em_srd:set_sipuri(Event1),
    ok = em_ema_server:create_hss_serviceprofile(Event1),
    ok = em_ema_server:create_hss_pubid(Event1),     
    
    case CPhone of
        "NODATA" -> ok;        
        _ ->
            ok = create_phone(Event1)
    end.
    
create_phone(#{ pubid := nil, user := User } = Event) ->
    Event1 = maps:update(pubid, User, Event),
    ok = em_srd:set_e164(Event1),
    ok = em_ema_server:create_hss_teluri(Event1),
    ok = em_ema_server:create_enum(Event1);
create_phone(Event) ->
    ok = em_srd:set_e164(Event),
    ok = em_ema_server:create_hss_teluri(Event),
    ok = em_ema_server:create_enum(Event).

delete_phone(#{ user := User, association := AId } = Event) ->
    CPhone = em_srd:get_e164(Event),
    ok = em_srd:delete_e164(User),
    ok = em_ema_server:delete_enum(CPhone),
    ok = em_ema_server:delete_hss_teluri(AId, CPhone).
        
delete_ims_association(Event) ->
    CPhone = em_srd:get_e164(Event),    
    case CPhone of
        "NODATA" -> ok;        
        _ ->
            ok = em_ema_server:delete_enum(CPhone)
    end,
    ok = em_srd:delete_user(Event),
    ok = em_ema_server:delete_hss_subscriber(Event).
        
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
     ?_assert(plan_phone_change("299123456", "299123456") =:= ignore),
     ?_assert(plan_phone_change("299123456", "NODATA") =:= create),
     ?_assert(plan_phone_change("299123456", "111111") =:= update)
    ].
-endif.    