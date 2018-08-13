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

-module(em_service).

-export([create/2, modify/2, delete/2, create_ims_association/3, modify_ims_association/3, delete_ims_association/3]).
-include("../include/em.hrl").


%%%===================================================================
%%% API
%%%===================================================================
% Attrs can hold following values:
%
% pubid: bob@home.net (default is USER)
% sprofile: bob@home.net (Default is PUBID)
% irs: "0" | "1" (default is "0")
% isdefault "true" | "false" (default is "false")
% phone: 666666 
% group: VK11111118
% type: user | virtual | pilot | trunk
% csprofile: user | virtual | trunk_pilot | trunk_ddi (default is TYPE)

create(Id, Message) ->
    Attrs = #{
        group       => get_group(Message),
        type        => "virtual"
    },
    process(create_ims_association, Id, get_user(Message), Attrs).

modify(Id, Message) ->
    User = get_user(Message),
    Attrs = #{
        pubid       => get_pubid(User, Message),
        phone       => get_phone(Message),
        type        => "virtual"
    },
    
    case get_event_modify_type(Message) of
        profile -> em_db:set_white_event(Id);
        address -> process(modify_ims_association, Id, User, Attrs)
    end.
    
delete(Id, Message) ->
    Attrs = #{},
    process(delete_ims_association, Id, get_user(Message), Attrs).

%%%===================================================================
%%% Internal functions
%%%===================================================================

% We are awaiting the result since all processing must be _strictly_ sequential.
process(F, Id, User, Attrs) ->
    {ok, C} = open_ema_session(),    
    {Pid, Ref} = spawn_monitor(?MODULE, F, [C, User, Attrs]),
    Reply = await_result(Pid, Ref, Id),
    em_ema:close(C),
    Reply.

create_ims_association(C, User, Attrs) ->
    ok = em_srd:create_user(User, Attrs),
    ok = em_hss_association:create(User, Attrs, C).
    
delete_ims_association(C, User, _Attrs) ->
    delete_phone(C, User, #{}),
    em_srd:delete_user(User, #{}),
    ok = em_hss_association:delete(User, C).    

modify_ims_association(C, User, Attrs) ->
    plan_pubid(C, User, Attrs),
    plan_phone(C, User, Attrs),
    ok.

%% ----- Handling of PUBID             
plan_pubid(C, User, Attrs) ->
    PubId = maps:get(pubid, Attrs),
    CPubId = em_srd:get_sipuri(User),      
    Plan = plan_pubid_change(PubId, CPubId),
    case Plan of
        ignore -> 
            logger:debug("No pubId plan found ~p", [User]),
            ok;
        update -> 
            update_pubid(C, User, Attrs)
    end.

update_pubid(C, User, Attrs) ->
    CurrentPubId = em_srd:get_sipuri(User),
    CurrentPhone = em_srd:get_phone(User),
    UpdatedAttrs = maps:put(phone, CurrentPhone, Attrs),
    
    case CurrentPhone of
        undefined ->
            logger:debug("Updating sipuri ~p", [User]),
            ok = em_hss_sipuri:delete(User, CurrentPubId, C),
            ok = em_hss_serviceprofile:delete(User, CurrentPubId, C),
            ok = em_hss_serviceprofile:create(User, Attrs, C),
            ok = em_hss_sipuri:create(User, Attrs, C),
            ok = em_srd:set_sipuri(User, Attrs);
        CurrentPhone ->
            logger:debug("Deleting phone and sipuri ~p", [User]),
            ok = em_dns_enum:delete(User, CurrentPhone, C),            
            ok = em_hss_teluri:delete(User, CurrentPhone, C),
            ok = em_hss_sipuri:delete(User, CurrentPubId, C),
            ok = em_hss_serviceprofile:delete(User, CurrentPubId, C),
            ok = em_hss_serviceprofile:create(User, Attrs, C),
            ok = em_hss_sipuri:create(User, Attrs, C),
            ok = em_srd:set_sipuri(User, Attrs),
            ok = em_hss_teluri:create(User, UpdatedAttrs, C),
            ok = em_dns_enum:create(User, UpdatedAttrs, C),
            ok = em_srd:set_phone(User, UpdatedAttrs)
    end.    


get_event_modify_type(Message) ->
    InsideCommand = em_utils:get_element_childs(Message),
    [ServiceInstanceProfile] = em_utils:get_elements(serviceInstanceProfile, InsideCommand),
    [Name] = em_utils:get_elements(name, em_utils:get_element_childs(ServiceInstanceProfile)),
    case em_utils:get_element_text(Name) of
        undefined -> address;
        _ -> profile
    end.


%% ----- Handling of PHONE          
plan_phone(C, User, Attrs) ->
    Phone = maps:get(phone, Attrs),
    CPhone = em_srd:get_phone(User),
    Plan = plan_phone_change(Phone, CPhone),
    
    case Plan of
        ignore ->
            logger:debug("No phone plan found ~p", [User]),
            ok;
        delete -> delete_phone(C, User, Attrs);
        update -> update_phone(C, User, Attrs)
    end.

update_phone(C, User, Attrs) ->
    case em_srd:get_phone(User) of
        undefined ->
            logger:debug("Creating phone ~p", [maps:get(phone, Attrs)]),
            ok = em_hss_teluri:create(User, Attrs, C),
            ok = em_dns_enum:create(User, Attrs, C),
            ok = em_srd:set_phone(User, Attrs);
        CurrentPhone ->
            logger:debug("Updating phone ~p", [CurrentPhone]),
            ok = em_hss_teluri:delete(User, CurrentPhone, C),
            ok = em_dns_enum:delete(User, CurrentPhone, C),
            ok = em_hss_teluri:create(User, Attrs, C),
            ok = em_dns_enum:create(User, Attrs, C),
            ok = em_srd:set_phone(User, Attrs)
    end.

delete_phone(C, User, _Attrs) ->
    case em_srd:get_phone(User) of
        undefined -> ok;
        CurrentPhone -> 
            logger:debug("Deleting phone ~p", [CurrentPhone]),
            ok = em_srd:delete_phone(User),
            ok = em_hss_teluri:delete(User, CurrentPhone, C),
            ok = em_dns_enum:delete(User, CurrentPhone, C)
    end.

get_user(Message) ->
    InsideCommand = em_utils:get_element_childs(Message),
    [ServiceUserId] = em_utils:get_elements(serviceUserId, InsideCommand),
    em_utils:get_element_text(ServiceUserId).

get_phone(Message) ->
    InsideCommand = em_utils:get_element_childs(Message),
    [ServiceInstanceProfile] = em_utils:get_elements(serviceInstanceProfile, InsideCommand),
    [PhoneNumber] = em_utils:get_elements(phoneNumber, em_utils:get_element_childs(ServiceInstanceProfile)),
    em_utils:get_element_text(PhoneNumber).

get_pubid(User, Message) ->
    InsideCommand = em_utils:get_element_childs(Message),
    [ServiceInstanceProfile] = em_utils:get_elements(serviceInstanceProfile, InsideCommand),
    [PublicUserIdentity] = em_utils:get_elements(publicUserIdentity, em_utils:get_element_childs(ServiceInstanceProfile)),
    fix_nil_pubid(PublicUserIdentity, User).


get_group(Message) ->
    InsideCommand = em_utils:get_element_childs(Message),
    [GroupId] = em_utils:get_elements(groupId, InsideCommand),
    em_utils:get_element_text(GroupId).


% Waiting for the process to finish.        
await_result(Pid, Ref, Id) ->
    receive
                
        {'DOWN', Ref, process, Pid, normal} ->
            logger:debug("Event processing successfull: ~p, ~p", [Ref, Pid]),
            logger:debug("Completing event: ~p", [Id]),
            em_db:complete_event(Id),
            ok;

        {'DOWN', Ref, process, Pid, Reason} ->
            logger:error("Event processing failed: ~p, ~p, ~p", [Ref, Pid, Reason]),
            fail_event(Id),
            error
            
    after 8000 ->
            logger:error("Event process timeout"),
            fail_event(Id),
            error
    end.      

open_ema_session() ->
    
    PriHost = econfig:get_value(em, "ema", "primary_host"),
    SecHost = econfig:get_value(em, "ema", "secondary_host"),
    User = econfig:get_value(em, "ema", "username"),
    Pass = econfig:get_value(em, "ema", "password"),    
    case em_ema:open(PriHost, User, Pass) of
        {ok, C} -> 
            logger:debug("EMA session to ~p established", [PriHost]),
            {ok, C};
        _Other -> 
            logger:debug("Attempting EMA session to ~p", [SecHost]),
            em_ema:open(SecHost, User, Pass)
    end.
    
% This is a "hack" in order to fit BW and IMS core together
fix_nil_pubid(Element, User) ->
    IsNil =  em_utils:get_element_attributes('xsi:nil', Element) =:= "true",
    Text = em_utils:get_element_text(Element),    
    
    case {Text, IsNil} of
        {undefined, false} -> undefined;
        {undefined, true} -> User;
        _ -> Text
    end.
    
fail_event(Id) ->
    em_db:fail_event(Id).
    
plan_phone_change(X, X) ->
    ignore;
plan_phone_change(undefined, _X) ->
    delete;
plan_phone_change(_X, undefined) ->
    update;
plan_phone_change(_X, _Y) ->
    update.   
    
plan_pubid_change(undefined, _Y) ->
    ignore;
plan_pubid_change(X, X) ->
    ignore;
plan_pubid_change(_X, _Y) ->
    update.