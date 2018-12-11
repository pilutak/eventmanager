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

-module(em_user).

-export([create/2, 
        modify/2, 
        delete/2, 
        delete_group/2, 
        set_password/2, 
        create_trunk/2, 
        modify_voiceportal/2, 
        create_ims_association/3, 
        modify_ims_association/3, 
        delete_ims_association/3, 
        set_sip_password/3, 
        modify_vp/3]).
        
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
    logger:debug("Creating user"),
    User = get_user(Message),           
    Attrs = #{
        group => get_group(Message),
        type => "user",
        phonecontext => get_phonecontext(Message)
    },
    process(create_ims_association, Id, User, Attrs).

modify(Id, Message) ->
    User = get_user(Message),
    case get_endpoint(Message) of
        profile -> em_db:set_white_event(Id);
        undefined ->
            logger:debug("Modify user, no endpoint"),
            Attrs = #{
                pubid => User,
                phone => get_phone(Message),
                type  => "user",
                irs   => "1",
                phonecontext => get_phonecontext(Message)
            },
            process(modify_ims_association, Id, User, Attrs);
            
        {access, PubId} ->
            logger:debug("Modify user, access endpoint"),
            Attrs = #{
                pubid => PubId,
                phone => get_phone(Message),
                type  => "user",
                irs   => "1",
                phonecontext => get_phonecontext(Message)
            },
            process(modify_ims_association, Id, User, Attrs);
                 
        {trunk, PubId} ->
            logger:debug("Modify user, trunk endpoint"),
            Attrs = #{
                pubid => PubId,
                phone => get_phone(Message),
                type  => "trunk",
                irs   => "0",
                phonecontext => get_phonecontext(Message)
            },
            process(modify_ims_association, Id, User, Attrs)
    end. 

delete(Id, Message) ->
    User = get_user(Message), 
    Attrs = #{},  
    process(delete_ims_association, Id, User, Attrs).
    
delete_group(Id, Message) ->
    GroupId = get_group(Message),
    Users = em_srd:get_users(GroupId),
    logger:debug("Deleting all users in group: ~p", [GroupId]),
    case Users of
        [] -> em_db:complete_event(Id); 
        _ ->
            lists:foreach(
            fun(I) ->
                {I1} = I,
                I2 = binary_to_list(I1),
                process(delete_ims_association, Id, I2, #{})
            end, Users)
    end.        
    
create_trunk(Id, Message) ->
    logger:debug("Creating Trunk Pilot user"),
    InsideCommand = em_utils:get_element_childs(Message),
    [GroupId] = em_utils:get_elements(groupId, InsideCommand),
    [SipPassWord] = em_utils:get_elements(sipAuthenticationPassword, InsideCommand),
    [PilotUser] = em_utils:get_elements(pilotUser, InsideCommand),
    [PilotUserId] = em_utils:get_elements(userId, em_utils:get_element_childs(PilotUser)),
    [LinePort] = em_utils:get_elements(linePort, em_utils:get_element_childs(PilotUser)),

    User = em_utils:get_element_text(PilotUserId),
    SipPass = em_utils:get_element_text(SipPassWord),
    PubId = em_utils:get_element_text(LinePort),
    GrpId = em_utils:get_element_text(GroupId),
             
    Attrs = #{
        user        => User,
        pubid       => PubId,
        group       => GrpId,
        type        => "pilot",
        pass        => SipPass,
        phonecontext=> "tg.gl"
    },    
    %em_processor_trunk:create_user(Event);
    process(create_ims_association, Id, User, Attrs).

set_password(Id, Message) ->
    logger:debug("Setting SIP password"),
    InsideCommand = em_utils:get_element_childs(Message),
    [U] = em_utils:get_elements(userId, InsideCommand),
    [P] = em_utils:get_elements(newPassword, InsideCommand),
    User = em_utils:get_element_text(U),
    Pass = em_utils:get_element_text(P),

    Attrs = #{
        pass => Pass
    },
    process(set_sip_password, Id, User, Attrs).


modify_voiceportal(Id, Message) ->
    InsideCommand = em_utils:get_element_childs(Message),
    [ServiceInstanceProfile] = em_utils:get_elements(serviceInstanceProfile, InsideCommand),
    [PubId] = em_utils:get_elements(publicUserIdentity, em_utils:get_element_childs(ServiceInstanceProfile)),

    User = fix_nil(PubId),    
    Attrs = #{
        pubid       => fix_nil(PubId),
        phone       => get_phone(Message),
        type        => "virtual",
        irs         => "0",
        group       => get_group(Message),
        phonecontext=> "tg.gl" 
    },
    
    process(modify_vp, Id, User, Attrs).
    
    
%%%===================================================================
%%% Internal functions
%%%===================================================================
process(F, Id, User, Attrs) ->
    {ok, C} = open_ema_session(),    
    {Pid, Ref} = spawn_monitor(?MODULE, F, [C, User, Attrs]),

    % Awaiting result, processing must be sequential.
    Reply = await_result(Pid, Ref, Id),
    em_ema:close(C),
    Reply.

create_ims_association(C, User, Attrs) ->
    em_srd:create_user(User, Attrs),
    ok = em_hss_association:create(User, Attrs, C).
    
delete_ims_association(C, User, Attrs) ->
    delete_voicemail(C, User, Attrs),
    delete_phone(C, User, Attrs),
    delete_user(C, User, Attrs).
    
modify_ims_association(C, User, Attrs) ->
    case em_srd:user_exists(User) of
        false -> exit(non_existing_user);
        true ->
            plan_type(C, User, Attrs),
            plan_pubid(C, User, Attrs),
            plan_phone(C, User, Attrs),
            plan_phonecontext(C, User, Attrs),
            ok
    end.

set_sip_password(C, User, Attrs) ->
    Password = maps:get(pass, Attrs),
    ok = em_hss_association:password(User, Password, C).  
    
modify_vp(C, User, Attrs) ->
    case em_srd:user_exists(User) of
        false ->
            em_srd:create_user(User, Attrs),
            ok = em_hss_association:create(User, Attrs, C); 
        true ->
            plan_pubid(C, User, Attrs),
            plan_phone(C, User, Attrs)
    end,
    ok.


%% ----- Planning             
plan_type(C, User, Attrs) ->
    Type = maps:get(type, Attrs),
    CurrentType = em_srd:get_type(#{user => User}),

    Plan = plan_type_change(Type, CurrentType),
    case Plan of
        ignore ->
            logger:debug("No type plan found ~p", [User]),
            ok;
        update -> change_usertype(C, User, Attrs, Type)
    end.

plan_phonecontext(C, User, Attrs) ->
    PhoneContext = maps:get(phonecontext, Attrs),
    CurrentPhoneContext = em_srd:get_phonecontext(User),      
    Plan = plan_phonecontext_change(PhoneContext, CurrentPhoneContext),
    logger:debug("Phonecontext plan for user ~p is ~p", [User, Plan]),
    case Plan of
        ignore ->
            logger:debug("No phonecontext plan found ~p", [User]),
            ok;
        update -> update_phonecontext(C, User, Attrs)
    end.

plan_pubid(C, User, Attrs) ->
    PubId = maps:get(pubid, Attrs),
    CPubId = em_srd:get_sipuri(User),      
    Plan = plan_pubid_change(PubId, CPubId),
    case Plan of
        ignore ->
            logger:debug("No pubId plan found ~p", [User]),
            ok;
        update -> update_pubid(C, User, Attrs)
    end.
    
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


%% ----- Handling of PUBID             
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

%% ----- Handling of PHONE          
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

delete_voicemail(_C, User, _Attrs) ->
    case em_srd:get_vmail_user(User) of
        undefined -> ok;
        VmailUser -> 
            logger:debug("Deleting voivemail account ~p", [VmailUser]),
            ok = em_srd:delete_vmail(User),
            em_surgemail:delete_account(#{user => VmailUser})
    end.


delete_user(C, User, Attrs) ->
    ok = em_srd:delete_user(User, Attrs),
    case em_hss_association:delete(User, C) of
        ok -> ok;
        error -> ok
    end.
    
change_usertype(C, User, Attrs, UserType) ->
    CurrentPubId = em_srd:get_sipuri(User),
    CurrentPhone = em_srd:get_phone(User),
    Group = em_srd:get_group(User),
    
    NewAttrs = #{type => UserType, phone => CurrentPhone, pubid => CurrentPubId, group => Group },
    UpdatedAttrs = maps:merge(Attrs, NewAttrs),
    
    case CurrentPhone of
        undefined ->
            logger:debug("Migration to ~p, no phone ~p", [UserType, User]),
            ok = em_srd:delete_user(User, #{}),
            ok = em_hss_association:delete(User, C),   
            ok = em_srd:create_user(User, UpdatedAttrs),
            ok = em_hss_association:create(User, UpdatedAttrs, C);            
        CurrentPhone ->
            logger:debug("Migration to ~p, with phone ~p", [UserType, User]),
            ok = em_srd:delete_user(User, #{}),
            ok = em_hss_association:delete(User, C),
            ok = em_dns_enum:delete(User, CurrentPhone, C),               
            ok = em_srd:create_user(User, UpdatedAttrs),
            ok = em_hss_association:create(User, UpdatedAttrs, C),
            ok = em_hss_teluri:create(User, UpdatedAttrs, C),
            ok = em_srd:set_phone(User, UpdatedAttrs),
            ok = em_dns_enum:create(User, UpdatedAttrs, C)
    end.    
    
update_phonecontext (C, User, Attrs) ->
   logger:debug("Updating phonecontext for user ~p", [User]),
       PhoneContext = maps:get(phonecontext, Attrs),
       ok = em_srd:set_phonecontext(User, PhoneContext),
       ok = em_hss_serviceprofile:phonecontext(User, Attrs, C).
    
await_result(Pid, Ref, Id) ->
    receive
                
        {'DOWN', Ref, process, Pid, normal} ->
            logger:debug("Event processing successfull: ~p", [Id]),
            em_db:complete_event(Id),
            ok;

        {'DOWN', Ref, process, Pid, Reason} ->
            logger:error("Event processing failed: ~p, ~p", [Id, Reason]),
            fail_event(Id),
            error
            
    after 8000 ->
            logger:error("Event process timeout: ~p", [Id]),
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

get_phonecontext(Message) ->
    InsideCommand = em_utils:get_element_childs(Message),
    [UserId] = em_utils:get_elements(userId, InsideCommand),
    [FirstName] = em_utils:get_elements(callingLineIdFirstName, InsideCommand),
    [Address] = em_utils:get_elements(address, InsideCommand),
    [StateOrProvince] = em_utils:get_elements(stateOrProvince, em_utils:get_element_childs(Address)),
    State = em_utils:get_element_text(StateOrProvince),
    User = em_utils:get_element_text(UserId),
    
    case em_utils:get_element_text(FirstName) of
        undefined -> 
                logger:debug("Phoneconxt from SRD"),
                em_srd:get_phonecontext(User);
        _ -> 
                logger:debug("Phoneconxt from profile"),
                maps:get(State, em_utils:phonecontexts(), "tg.gl")
    end.

get_endpoint(Message) ->
    InsideCommand = em_utils:get_element_childs(Message),
    [Endpoint] = em_utils:get_elements(endpoint, InsideCommand),
    IsNil =  em_utils:get_element_attributes('xsi:nil', Endpoint) =:= "true",
     
    case sip_alias_exists(Message) of
        true ->
            case IsNil of
                true -> undefined;
                false -> get_endpoint(access, Endpoint)
            end;
        false -> profile
    end.
    
get_endpoint(access, Endpoint) ->
    [AccessDeviceEndpoint] = em_utils:get_elements(accessDeviceEndpoint, em_utils:get_element_childs(Endpoint)),
    [LinePort] = em_utils:get_elements(linePort, em_utils:get_element_childs(AccessDeviceEndpoint)),
    case em_utils:get_element_text(LinePort) of
        undefined -> get_endpoint(trunk, Endpoint);
        Text -> {access, Text}
    end; 
get_endpoint(trunk, Endpoint) ->        
    [TrunkAddressing] = em_utils:get_elements(trunkAddressing, em_utils:get_element_childs(Endpoint)),
    [TrunkGroupDeviceEndpoint] = em_utils:get_elements(trunkGroupDeviceEndpoint, em_utils:get_element_childs(TrunkAddressing)),
    [LinePort] = em_utils:get_elements(linePort, em_utils:get_element_childs(TrunkGroupDeviceEndpoint)),
    case em_utils:get_element_text(LinePort) of
        undefined -> undefined;
        Text -> {trunk, Text}
    end.

get_user(Message) ->
    InsideCommand = em_utils:get_element_childs(Message),
    [UserId] = em_utils:get_elements(userId, InsideCommand),
    em_utils:get_element_text(UserId).

get_phone(Message) ->
    InsideCommand = em_utils:get_element_childs(Message),
    [PhoneNumber] = em_utils:get_elements(phoneNumber, InsideCommand),
    em_utils:get_element_text(PhoneNumber).

get_group(Message) ->
    InsideCommand = em_utils:get_element_childs(Message),
    [GroupId] = em_utils:get_elements(groupId, InsideCommand),
    em_utils:get_element_text(GroupId).

sip_alias_exists(Message) ->
    InsideCommand = em_utils:get_element_childs(Message),
    [SipAliasList] = em_utils:get_elements(sipAliasList, InsideCommand),
    [SipAlias] = em_utils:get_elements(sipAlias, em_utils:get_element_childs(SipAliasList)),
    Alias = em_utils:get_element_text(SipAlias),
    
    SipAliasIsNil =  em_utils:get_element_attributes('xsi:nil', SipAliasList) =:= "true",
    
    case SipAliasIsNil of
        true -> 
            true;
        false ->
            %logger:debug("DEBUG SipAlias value is ~p", [Alias]),
            case Alias of
                undefined -> false;
                _ -> true
            end
        end.
    
fail_event(Id) ->
    em_db:fail_event(Id).

plan_phonecontext_change(X, X) ->
    ignore;
plan_phonecontext_change(_X, _Y) ->
    update.   

plan_type_change(X, X) ->
    ignore;
plan_type_change(_X, _Y) ->
    update.   

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
    
fix_nil(Element) ->
    IsNil =  em_utils:get_element_attributes('xsi:nil', Element) =:= "true",
    Text = em_utils:get_element_text(Element),    
    
    case {Text, IsNil} of
        {undefined, false} -> undefined;
        {undefined, true} -> nil;
        _ -> Text
    end.