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
    logger:info("Creating user: ~p", [maps:get(user, Event)]),
    create_ims_association(Event),
    logger:info("Created user: ~p", [maps:get(user, Event)]).
    
delete_user(Event) ->
    logger:info("Deleting user: ~p", [maps:get(user, Event)]),
    delete_ims_association(Event),
    logger:info("Deleted user: ~p", [maps:get(user, Event)]).
    
modify_user(Event) ->
    logger:info("Modifying user: ~p", [maps:get(user, Event)]),    
    CurrentUserType = em_srd:get_type(Event),
    User = maps:get(user, Event),
    Group = em_srd:get_group(User),
    PhoneContext = em_srd:get_phonecontext(User),

    case CurrentUserType of
        "trunk" -> 
            logger:info("Modify trunk user type to user"),
            delete_user(Event),
            Event5 = maps:put(group, Group, Event),
            Event6 = maps:update(pubid, User, Event5),
            Event7 = maps:update(sprofile, User, Event6),
            Event8 = maps:put(pass, em_utils:randchar(14), Event7),
            Event9 = maps:put(phonecontext, PhoneContext, Event8),
            create_user(Event9),
            Plan = make_plan(Event9),
            execute_plan(Plan, Event9);  
            
        "pilot" -> 
            logger:info("Modify pilot user type to user"),
            delete_user(Event),
            Event5 = maps:put(group, Group, Event),
            Event6 = maps:update(pubid, User, Event5),
            Event7 = maps:update(sprofile, User, Event6),
            Event8 = maps:put(pass, em_utils:randchar(14), Event7),
            create_user(Event8),
            Plan = make_plan(Event8),
            execute_plan(Plan, Event8);
            
        "user" ->    logger:info("Modify type is user"),
                     Plan1 = make_plan(Event),
                     execute_plan(Plan1, Event);
        "virtual" -> logger:info("Modify type is virtual"), 
                     Plan1 = make_plan(Event),
                     execute_plan(Plan1, Event)
    end.
    
modify_trunk_user(Event) ->
    logger:notice("Modifying trunk user: ~p", [maps:get(user, Event)]),    
    CurrentUserType = em_srd:get_type(Event),
    User = maps:get(user, Event),
    Group = em_srd:get_group(User),
    
    case CurrentUserType of
        "user" -> 
            logger:info("Changing user type to trunk: ~p", [User]),
            delete_user(Event),
            Event1 = maps:put(group, Group, Event),
            create_user(Event1),
            Plan1 = make_plan(Event1),
            execute_plan(Plan1, Event1);
        "trunk" -> logger:info("Already user type trunk: ~p", [User]),
                Plan2 = make_plan(Event),
                execute_plan(Plan2, Event);
        "pilot" -> logger:info("Already user type pilot: ~p", [User]),
                Event5 = maps:put(type, 'pilot', Event),
                Event6 = maps:update(csprofile, serviceprofile(trunk_pilot), Event5),
                Plan2 = make_plan(Event6),
                execute_plan(Plan2, Event6)
        
    end.   
    
set_password(#{ user := User} = Event) ->
    logger:info("Set SIP password for: ~p", [User]),
    case open_ema_session() of
        {ok, C} -> 
            {ok, _} = do_update_sip_password(C, Event),
            close_ema_session(C);
        Error -> Error
    end. 
                       
set_phonecontext(#{ user := User} = Event ) ->
    logger:info("Set phonecontext for: ~p", [User]),
    PubId = em_srd:get_sipuri(Event),
    Event1 = maps:put(pubid, PubId, Event),
    
    case open_ema_session() of
        {ok, C} -> 
            {ok, _} = do_update_context(C, Event1),
            close_ema_session(C);
        Error -> Error
    end.
    
%%%===================================================================
%%% Internal functions
%%%===================================================================
make_plan(#{ user := User, pubid := PubId, phone := Phone } = Event) ->
    logger:info("Making plan for: ~p", [User]), 
    CPubId = em_srd:get_sipuri(Event),
    CPhone = em_srd:get_e164(Event),

    PubIdAction = plan_pubid_change(PubId, CPubId, User),
    PhoneAction = plan_phone_change(Phone, CPhone),
    
    {PubIdAction, PhoneAction}.

execute_plan(Plan, Event) ->
    logger:info("Executing plan: ~p", [Plan]), 
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
                                                
        Err -> logger:error("No execution found for plan: ~p", [Err])
    end.

create_ims_association(#{ phone := "NODATA"} = Event) ->    
    case open_ema_session() of
        {ok, C} ->
            {ok, _} = do_create_subscriber(C, Event),
            close_ema_session(C);
        Error -> Error
    end;

create_ims_association(#{ phone := nil} = Event) ->    
    case open_ema_session() of
        {ok, C} ->
            {ok, _} = do_create_subscriber(C, Event),
            close_ema_session(C);
        Error -> Error
    end;
         
create_ims_association(Event) ->    
    case open_ema_session() of
        {ok, C} ->
            {ok, _} = do_create_subscriber(C, Event),
            {ok, _} = do_add_phone_hss(C, Event),
            {ok, _} = do_add_enum(C, Event),
            close_ema_session(C);
        Error -> Error
    end.
    
% copy from service
delete_phone(#{ user := User, association := AId } = Event) ->
    CPhone = em_srd:get_e164(Event),
    ok = em_srd:delete_e164(User),
    {ok, C} = open_ema_session(),
    Req = em_cai3g:delete_ims_enum(CPhone),
    {ok, _} = send_to_ema(C, Req),
    Req1 = em_cai3g:delete_ims_teluri(AId, CPhone),
    {ok, _} = send_to_ema(C, Req1),
    close_ema_session(C).
 
% special handling of users
create_phone(#{ pubid := nil, user := User } = Event) ->
    Event1 = maps:update(pubid, User, Event),
    ok = em_srd:set_e164(Event1),
    {ok, C} = open_ema_session(),
    Req1 = em_cai3g:add_ims_teluri(Event1),
    {ok, _} = send_to_ema(C, Req1),    
    Req2 = em_cai3g:add_ims_enum(Event1),
    {ok, _} = send_to_ema(C, Req2),
    close_ema_session(C);
    
create_phone(#{ pubid := undefined, user := User } = Event) ->
    Event1 = maps:update(pubid, User, Event),
    ok = em_srd:set_e164(Event1),
    {ok, C} = open_ema_session(),
    Req1 = em_cai3g:add_ims_teluri(Event1),
    {ok, _} = send_to_ema(C, Req1),    
    Req2 = em_cai3g:add_ims_enum(Event1),
    {ok, _} = send_to_ema(C, Req2),
    close_ema_session(C);
    
create_phone(Event) ->
    ok = em_srd:set_e164(Event),
    {ok, C} = open_ema_session(),
    Req1 = em_cai3g:add_ims_teluri(Event),
    {ok, _} = send_to_ema(C, Req1),    
    Req2 = em_cai3g:add_ims_enum(Event),
    {ok, _} = send_to_ema(C, Req2),
    close_ema_session(C).

delete_impu_sip(#{ user := User, association := AId } = Event) ->
    CPubId = em_srd:get_sipuri(Event),
    CPhone = em_srd:get_e164(Event),
    CContext = em_srd:get_phonecontext(User),
    
    case CPhone of
        "NODATA" -> ok;        
        _ ->
            delete_phone(Event)
    end,

    {ok, C} = open_ema_session(),
    Req1 = em_cai3g:delete_ims_pubid(AId, CPubId),
    {ok, _} = send_to_ema(C, Req1),    
    Req2 = em_cai3g:delete_ims_serviceprofile(AId, CPubId),
    {ok, _} = send_to_ema(C, Req2),
        
    Event1 = maps:update(pubid, User, Event),
    Event2 = maps:put(phonecontext, CContext, Event1),
    
    ok = em_srd:set_sipuri(Event2),
    Req3 = em_cai3g:add_ims_serviceprofile(Event2),
    {ok, _} = send_to_ema(C, Req3),    
    Req4 = em_cai3g:add_ims_pubid(Event2),
    {ok, _} = send_to_ema(C, Req4),
    close_ema_session(C),
      
    case CPhone of
        "NODATA" -> ok;        
        _ ->
            ok = create_phone(Event2)
    end.

% special handling of users
update_impu_sip(#{ user := User, association := AId, pubid := undefined } = Event) ->
    CPubId = em_srd:get_sipuri(Event),
    CPhone = em_srd:get_e164(Event),
    CContext = em_srd:get_phonecontext(User),
    
    case CPhone of
        "NODATA" -> ok;        
        _ ->
            delete_phone(Event)
    end,


    {ok, C} = open_ema_session(),
    Req1 = em_cai3g:delete_ims_pubid(AId, CPubId),
    {ok, _} = send_to_ema(C, Req1),    
    Req2 = em_cai3g:delete_ims_serviceprofile(AId, CPubId),
    {ok, _} = send_to_ema(C, Req2),    
    
    Event1 = maps:update(pubid, User, Event),
    Event2 = maps:put(phonecontext, CContext, Event1),
    
    ok = em_srd:set_sipuri(Event2),
    Req3 = em_cai3g:add_ims_serviceprofile(Event2),
    {ok, _} = send_to_ema(C, Req3),    

    Req4 = em_cai3g:add_ims_pubid(Event2),
    {ok, _} = send_to_ema(C, Req4),  
    close_ema_session(C),  
    
    case CPhone of
        "NODATA" -> ok;        
        _ ->
            Event2 = maps:update(phone, CPhone, Event2),
            ok = create_phone(Event2)
    end;

update_impu_sip(#{ user := User, association := AId } = Event) ->
    CPubId = em_srd:get_sipuri(Event),
    logger:notice("CPUBID is:~p", [CPubId]),
    CPhone = em_srd:get_e164(Event),
    CContext = em_srd:get_phonecontext(User),
    
    case CPhone of
        "NODATA" -> ok;        
        _ ->
            delete_phone(Event)
    end,

    {ok, C} = open_ema_session(),
    Req1 = em_cai3g:delete_ims_pubid(AId, CPubId),
    {ok, _} = send_to_ema(C, Req1),    
        
    Req2 = em_cai3g:delete_ims_serviceprofile(AId, CPubId),
    {ok, _} = send_to_ema(C, Req2), 
    
    Event1 = maps:put(phonecontext, CContext, Event),
    ok = em_srd:set_sipuri(Event1),
    Req3 = em_cai3g:add_ims_serviceprofile(Event1),
    {ok, _} = send_to_ema(C, Req3),    

    Req4 = em_cai3g:add_ims_pubid(Event1),
    {ok, _} = send_to_ema(C, Req4),
    close_ema_session(C), 
    
    case CPhone of
        "NODATA" -> ok;        
        _ ->
            Event2 = maps:update(phone, CPhone, Event1),
            ok = create_phone(Event2)
    end.

    
delete_ims_association(Event) ->
    case open_ema_session() of
       {ok, C} ->
           {ok, _} = do_delete_enum(C, Event),
           {ok, _} = do_delete_subscriber(C, Event),

           close_ema_session(C);
        Error -> Error
    end.    

%% DO functions
%% -------------------------------------------------------------------
do_delete_subscriber(C, Event) ->
    ok = em_srd:delete_user(Event),
    Req = em_cai3g:delete_ims_subscriber(Event),
    send_to_ema(C, Req).
        
%do_delete_phone_hss(C, Event) ->
%    User = maps:get(user, Event),
%    AssociationId = maps:get(association, Event),
%    Phone = em_srd:get_e164(Event),
%    
%    case get_srd_phone(Event) of
%        undefined -> {error, undefined};
%        Phone ->
%            ok = em_srd:delete_e164(User),
%            Req = em_cai3g:delete_ims_teluri(AssociationId, Phone),
%            send_to_ema(C, Req)
%    end.

do_delete_enum(C, Event) ->
    case get_srd_phone(Event) of
        undefined -> {ok, undefined};
        Phone ->
            Req = em_cai3g:delete_ims_enum(Phone),
            send_to_ema(C, Req)
    end.

do_update_sip_password(C, Event) ->
    Req = em_cai3g:set_ims_pass(Event),    
    send_to_ema(C, Req).

do_update_context(C, Event ) ->
    User = maps:get(user, Event),
    PhoneContext = maps:get(phonecontext, Event),
    ok = em_srd:set_phonecontext(User, PhoneContext),
    Req = em_cai3g:set_ims_phonecontext(Event),
    send_to_ema(C, Req).
    
%do_delete_pubid(C, Event) ->
%    AssociationId = maps:get(association, Event),
%    CPubId = em_srd:get_sipuri(Event),
%    Req = em_cai3g:delete_ims_pubid(AssociationId, CPubId),
%    send_to_ema(C, Req).
        
%do_delete_serviceprofile(C, Event) ->
%    AssociationId = maps:get(association, Event),
%    CPubId = em_srd:get_sipuri(Event),
%    Req = em_cai3g:delete_ims_serviceprofile(AssociationId, CPubId),
%    send_to_ema(C, Req).   
    
%do_add_serviceprofile(C, Event) ->
%    Req = em_cai3g:add_ims_serviceprofile(Event),
%    send_to_ema(C, Req).    

%do_add_pubid(C, Event) ->
%    ok = em_srd:set_sipuri(Event),
%    Req = em_cai3g:add_ims_pubid(Event),
%    send_to_ema(C, Req).    
    
do_add_phone_hss(C, Event) ->
    ok = em_srd:set_e164(Event),
    Req = em_cai3g:add_ims_teluri(Event),
    send_to_ema(C, Req).

do_add_enum(C, Event) ->
    Req = em_cai3g:add_ims_enum(Event),
    send_to_ema(C, Req).
    
do_create_subscriber(C, #{type := "user"} = Event) ->
    ok = em_srd:create_user(Event),
    Req = em_cai3g:add_ims_subscriber(Event),
    send_to_ema(C, Req);
do_create_subscriber(C, #{type := "pilot"} = Event) ->
    ok = em_srd:create_user(Event),
    Req = em_cai3g:add_ims_subscriber(Event),
    send_to_ema(C, Req);
do_create_subscriber(C, #{type := "virtual"} = Event) ->
    ok = em_srd:create_user(Event),
    Req = em_cai3g:add_ims_virtual_subscriber(Event),
    send_to_ema(C, Req);
do_create_subscriber(C, #{type := "trunk"} = Event) ->
    ok = em_srd:create_user(Event),
    Req = em_cai3g:add_ims_virtual_subscriber(Event),
    send_to_ema(C, Req).
        
%% GET functions
%% -------------------------------------------------------------------
get_srd_phone(Event) ->
    case em_srd:get_e164(Event) of
        "NODATA" -> undefined;
        Phone -> Phone    
    end.
    
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

serviceprofile(Id) ->
    Profiles = econfig:get_value(em, "service_profiles"),
    proplists:get_value(Id, Profiles).
    
open_ema_session() ->
    PriHost = econfig:get_value(em, "ema", "primary_host"),
    SecHost = econfig:get_value(em, "ema", "secondary_host"),
    User = econfig:get_value(em, "ema", "username"),
    Pass = econfig:get_value(em, "ema", "password"),
        
    case em_ema:open(PriHost, User, Pass) of
        {ok, C} -> {ok, C};
        _Other -> em_ema:open(SecHost, User, Pass)
    end.
        
close_ema_session(C) ->
    em_ema:close(C).
    
send_to_ema(C, Req) ->
    Resp = em_ema:send(C, Req),
    case Resp of
        {ok, Payload} -> {ok, Payload};
        {error, {4006, 13005}} -> logger:info("Association do not exist: ~p", [Resp]),
                                    {ok, "Association do not exist"};
        Other -> logger:error("EMA request error: ~p", [Other]),
            Other
    end.
   
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
   
