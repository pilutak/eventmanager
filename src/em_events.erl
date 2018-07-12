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

-module(em_events).

-export([process/3]).
-export([processors/0]).
-include("../include/em.hrl").


%%%===================================================================
%%% API
%%%===================================================================
process(Id, CommandType, Message) ->
    Processor = maps:get(CommandType, processors(), ignored),
    processor(Id, Processor, Message).

processor(Id, create_service, Message) ->
    InsideCommand = em_utils:get_element_childs(Message),
    [ServiceUserId] = em_utils:get_elements(serviceUserId, InsideCommand),
    [GroupId] = em_utils:get_elements(groupId, InsideCommand),
    
    User = em_utils:get_element_text(ServiceUserId),
    Event = #{
        user        => em_utils:get_element_text(ServiceUserId),
        pubid       => em_utils:get_element_text(ServiceUserId),
        group       => em_utils:get_element_text(GroupId),
        type        => "virtual",
        csprofile   => serviceprofile("virtual"),
        ispsi       => "true",
        isdefault   => "false",
        irs         => "0",
        association => em_utils:md5_hex(User),
        phone       => "NODATA",
        phonecontext=> "tg.gl"
        
    },
    em_manager_hss:create_user(Event),
    em_db:complete_event(Id);

processor(Id, modify_service, Message) ->
    InsideCommand = em_utils:get_element_childs(Message),
    [ServiceUserId] = em_utils:get_elements(serviceUserId, InsideCommand),
    [ServiceInstanceProfile] = em_utils:get_elements(serviceInstanceProfile, InsideCommand),
    [PhoneNumber] = em_utils:get_elements(phoneNumber, em_utils:get_element_childs(ServiceInstanceProfile)),
    [PublicUserIdentity] = em_utils:get_elements(publicUserIdentity, em_utils:get_element_childs(ServiceInstanceProfile)),

    User = em_utils:get_element_text(ServiceUserId),
    Event = #{
        user        => em_utils:get_element_text(ServiceUserId),
        pubid       => fix_nil(PublicUserIdentity),
        phone       => fix_nil(PhoneNumber),
        type        => "virtual",
        csprofile   => serviceprofile("virtual"),
        ispsi       => "true",
        isdefault   => "false",
        irs         => "0",
        association => em_utils:md5_hex(User)
    },
    
    % In order not to have the "profile" CommPilot page to trigger modify, we check
    % if the PubId field is present, if not, the request is ignored.
    case fix_nil(PublicUserIdentity) of
        undefined -> ok;
        _ -> ok = em_manager_hss:modify_user(Event)
    end,
    em_db:complete_event(Id);
        
processor(Id, modify_group_vp, Message) ->
    InsideCommand = em_utils:get_element_childs(Message),
    [GroupId] = em_utils:get_elements(groupId, InsideCommand),
    [ServiceInstanceProfile] = em_utils:get_elements(serviceInstanceProfile, InsideCommand),
    [Phone] = em_utils:get_elements(phoneNumber, em_utils:get_element_childs(ServiceInstanceProfile)),
    [PubId] = em_utils:get_elements(publicUserIdentity, em_utils:get_element_childs(ServiceInstanceProfile)),

    User = fix_nil(PubId),
    
    Event = #{
        user        => User,
        pubid       => fix_nil(PubId),
        phone       => fix_nil(Phone),
        type        => "virtual",
        csprofile   => serviceprofile("virtual"),
        ispsi       => "true",
        isdefault   => "false",
        irs         => "0",
        group       => fix_nil(GroupId),
        association => em_utils:md5_hex(User),
        phonecontext=> "tg.gl" 
    },
    
    case em_srd:user_exists(User) of
        false ->
            em_manager_hss:create_user(Event);
            
        true ->
            em_manager_hss:modify_user(Event)
    end,
    em_db:complete_event(Id);

processor(Id, modify_user_vm, Message) ->
    logger:notice("Start processing vmail"), 
    InsideCommand = em_utils:get_element_childs(Message),
    [U] = em_utils:get_elements(userId, InsideCommand),
    [G] = em_utils:get_elements(groupMailServerUserId, InsideCommand),
    [P] = em_utils:get_elements(groupMailServerPassword, InsideCommand),
    UserName = em_utils:get_element_text(U),
    MailUser = em_utils:get_element_text(G),
    MailPass = em_utils:get_element_text(P),
    logger:notice("Processing vmail, LOADING EVENT"), 
                    
    Event = #{
        user        => UserName,
        mailuser    => MailUser,
        mailpass    => MailPass,
        current_mailuser   => em_srd:get_vmail_user(UserName),
        current_mailpass   => em_srd:get_vmail_pass(UserName)
    },
    em_manager_surgemail:modify(Event),
    em_db:complete_event(Id);
        
processor(Id, create_user, Message) ->
    InsideCommand = em_utils:get_element_childs(Message),
    [UserId] = em_utils:get_elements(userId, InsideCommand),
    [GroupId] = em_utils:get_elements(groupId, InsideCommand),

    % Fetch address/state (used for 112/113 emergency routing)
    [AD] = em_utils:get_elements(address, InsideCommand),
    [SP] = em_utils:get_elements(stateOrProvince, em_utils:get_element_childs(AD)),
    City = em_utils:get_element_text(SP),
    PhoneContext = maps:get(City, phonecontexts(), "tg.gl"),


    User = em_utils:get_element_text(UserId),        
    Event = #{
        user        => User,
        pubid       => User,
        group       => em_utils:get_element_text(GroupId),
        type        => "user",
        csprofile   => serviceprofile("user"),
        ispsi       => "false",
        isdefault   => "false",
        irs         => "1",
        association => em_utils:md5_hex(User),
        phone       => "NODATA",
        pass        => em_utils:randchar(14),
        phonecontext=> PhoneContext
    },
    ok = em_manager_hss:create_user(Event),
    em_db:complete_event(Id);
    
    
processor(Id, modify_user, Message) ->
    InsideCommand = em_utils:get_element_childs(Message),
    [U] = em_utils:get_elements(userId, InsideCommand),
    [P] = em_utils:get_elements(phoneNumber, InsideCommand),

    % Fetch endpoint data
    [E] = em_utils:get_elements(endpoint, InsideCommand),
    [A] = em_utils:get_elements(accessDeviceEndpoint, em_utils:get_element_childs(E)),
    [L] = em_utils:get_elements(linePort, em_utils:get_element_childs(A)),

    % Fetch address/state (used for 112/113 emergency routing)
    [AD] = em_utils:get_elements(address, InsideCommand),
    [SP] = em_utils:get_elements(stateOrProvince, em_utils:get_element_childs(AD)),
    %StateProvince = em_utils:get_element_text(SP),
    StateProvince = fix_nil1(SP),
    
    %PhoneContext = maps:get(City1, phonecontexts(), "tg.gl"),
        
    % Fecth endpoint Trunk data
    [F] = em_utils:get_elements(endpoint, InsideCommand),
    [T] = em_utils:get_elements(trunkAddressing, em_utils:get_element_childs(F)),
    [TG] = em_utils:get_elements(trunkGroupDeviceEndpoint, em_utils:get_element_childs(T)),
    [LP] = em_utils:get_elements(linePort, em_utils:get_element_childs(TG)),
    TrunkLinePort = em_utils:get_element_text(LP),
    
    UserName = em_utils:get_element_text(U),
    Phone = fix_nil(P),

    case StateProvince of
        undefined -> ok;
        nil ->
            ContextEvent = #{
                user => UserName,
                association => em_utils:md5_hex(UserName),
                phonecontext => "tg.gl"
                }, 
            em_manager_hss:set_phonecontext(ContextEvent);        
        _-> 
            PhoneContext = maps:get(StateProvince, phonecontexts(), undefined),
            ContextEvent = #{
                user => UserName,
                association => em_utils:md5_hex(UserName),
                phonecontext => PhoneContext
                }, 
            em_manager_hss:set_phonecontext(ContextEvent)
    end,

    case TrunkLinePort of
         undefined when Phone == undefined ->
             ok;
         undefined when Phone /= undefined ->
              logger:notice("Modify user"),
              Event = #{
                  user        => UserName,
                  pubid       => fix_nil(L),
                  phone       => fix_nil(P),
                  type        => "user",
                  csprofile   => serviceprofile("user"),
                  ispsi       => "false",
                  isdefault   => "false",
                  irs         => "1",
                  association => em_utils:md5_hex(UserName),
                  sprofile    => fix_nil(L)
              },
              em_manager_hss:modify_user(Event);
                  
         _  -> 
             logger:notice("Modify trunk user"),
             Event1 = #{
                 user        => UserName,
                 pubid       => fix_nil(LP),
                 phone       => fix_nil(P),
                 type        => "trunk",
                 csprofile   => serviceprofile("trunk_ddi"),
                 ispsi       => "true",
                 isdefault   => "false",
                 irs         => "0",
                 association => em_utils:md5_hex(UserName),
                 sprofile    => fix_nil(LP)
             },
             em_manager_hss:modify_trunk_user(Event1)
             
     end,
     em_db:complete_event(Id);  
         

processor(Id, delete_service, Message) ->
    InsideCommand = em_utils:get_element_childs(Message),
    [ServiceUserId] = em_utils:get_elements(serviceUserId, InsideCommand),
    User = em_utils:get_element_text(ServiceUserId),
    
    Event = #{
        user => User,
        association => em_utils:md5_hex(User)
    },
    em_manager_hss:delete_user(Event),
    em_db:complete_event(Id);

processor(Id, delete_user, Message) ->
    InsideCommand = em_utils:get_element_childs(Message),
    [UserId] = em_utils:get_elements(userId, InsideCommand),
    User = em_utils:get_element_text(UserId),

    Event = #{
        user => User,
        association => em_utils:md5_hex(User)
    },
    
    em_manager_surgemail:delete_account(Event),
    em_manager_hss:delete_user(Event),
    em_db:complete_event(Id);

processor(Id, set_password, Message) ->
    InsideCommand = em_utils:get_element_childs(Message),
    [U] = em_utils:get_elements(userId, InsideCommand),
    [P] = em_utils:get_elements(newPassword, InsideCommand),
    UserName = em_utils:get_element_text(U),
    Pass = em_utils:get_element_text(P),

    Event = #{
        user => UserName,
        association => em_utils:md5_hex(UserName),
        pass => Pass
    },
    em_manager_hss:set_password(Event),
    em_db:complete_event(Id);
    
processor(Id, create_trunk, Message) ->
    InsideCommand = em_utils:get_element_childs(Message),
    [GroupId] = em_utils:get_elements(groupId, InsideCommand),
    [SipPassWord] = em_utils:get_elements(sipAuthenticationPassword, InsideCommand),
    [PilotUser] = em_utils:get_elements(pilotUser, InsideCommand),
    [PilotUserId] = em_utils:get_elements(userId, em_utils:get_element_childs(PilotUser)),
    [LinePort] = em_utils:get_elements(linePort, em_utils:get_element_childs(PilotUser)),
    UserName = em_utils:get_element_text(PilotUserId),
    SipPass = em_utils:get_element_text(SipPassWord),
    PubId = em_utils:get_element_text(LinePort),
    GrpId = em_utils:get_element_text(GroupId),
             
    Event = #{
        user        => UserName,
        pubid       => PubId,
        group       => GrpId,
        type        => "pilot",
        csprofile   => serviceprofile("trunk_pilot"),
        ispsi       => "false",
        isdefault   => "false",
        irs         => "1",
        association => em_utils:md5_hex(UserName),
        phone       => "NODATA",
        pass        => SipPass,
        phonecontext=> "tg.gl"
    },    
    %em_processor_trunk:create_user(Event);
    em_manager_hss:create_user(Event),
    em_db:complete_event(Id);
    

processor(Id, delete_group, Message) ->
    InsideCommand = em_utils:get_element_childs(Message),
    [G] = em_utils:get_elements(groupId, InsideCommand),
    GroupId = em_utils:get_element_text(G),
    Users = em_srd:get_users(GroupId),
    
    logger:notice("Deleting all users in group: ~p", [GroupId]),
    lists:foreach(
        fun(I) ->
            {I1} = I,
            I2 = binary_to_list(I1),
            em_manager_surgemail:delete_account(#{user => I2}),
            em_manager_hss:delete_user(#{user => I2, association => em_utils:md5_hex(I2)})
        end, Users),
    em_db:complete_event(Id);

processor(Id, create_domain, Message) ->
    InsideCommand = em_utils:get_element_childs(Message),
    [D] = em_utils:get_elements(domain, InsideCommand),
    Domain = em_utils:get_element_text(D),
    em_manager_surgemail:create_domain(Domain),
    em_db:complete_event(Id);

processor(Id, delete_domain, Message) ->
    InsideCommand = em_utils:get_element_childs(Message),
    [D] = em_utils:get_elements(domain, InsideCommand),
    Domain = em_utils:get_element_text(D),
    em_manager_surgemail:delete_domain(Domain),
    em_db:complete_event(Id);

processor(_Id, ignored, _Message) ->
    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================
fix_nil(Element) ->
    IsNil =  em_utils:get_element_attributes('xsi:nil', Element) =:= "true",
    Text = em_utils:get_element_text(Element),    
    
    case {Text, IsNil} of
        {undefined, false} -> undefined;
        {undefined, true} -> nil;
        _ -> Text
    end.

fix_nil1(Element) ->
    IsNil =  em_utils:get_element_attributes('xsi:nil', Element) =:= "true",
    io:format(IsNil),
    Text = em_utils:get_element_text(Element),    
    
    case {Text, IsNil} of
        {undefined, false} -> undefined;
        {undefined, true} -> nil;
        _ -> Text
    end.

         
%% mapping various OCI-R events to functions
processors() ->
    #{  
        "GroupAutoAttendantAddInstanceRequest20" => create_service,
        "GroupAutoAttendantModifyInstanceRequest20" => modify_service,
        "GroupAutoAttendantDeleteInstanceRequest" => delete_service,
        "GroupHuntGroupAddInstanceRequest20" => create_service,
        "GroupHuntGroupModifyInstanceRequest" => modify_service,
        "GroupHuntGroupDeleteInstanceRequest" => delete_service,
        "GroupCallCenterAddInstanceRequest19" => create_service,
        "GroupCallCenterModifyInstanceRequest19" => modify_service,
        "GroupCallCenterDeleteInstanceRequest" => delete_service,
        "GroupMeetMeConferencingAddInstanceRequest19" => create_service,
        "GroupMeetMeConferencingModifyInstanceRequest" => modify_service,
        "GroupMeetMeConferencingDeleteInstanceRequest" => delete_service,
        "UserAddRequest17sp4" => create_user,
        "UserModifyRequest17sp4" => modify_user,
        "UserDeleteRequest" => delete_user,
        "UserAuthenticationModifyRequest" => set_password,
        "GroupTrunkGroupAddInstanceRequest21" => create_trunk,
        "GroupVoiceMessagingGroupModifyVoicePortalRequest" => modify_group_vp,
        "UserVoiceMessagingUserModifyAdvancedVoiceManagementRequest" => modify_user_vm,
        "GroupDeleteRequest" => delete_group,
        "SystemDomainAddRequest" => create_domain,
        "SystemDomainDeleteRequest" => delete_domain
    }.

% The OCI-R event contains the city name, here we translate
% to the phonecontext used in the HSS
phonecontexts() ->
    #{
        "Nuuk" => "nuk.tg.gl",
        "Nanortalik" => "nan.tg.gl",
        "Narsaq" => "nar.tg.gl",
        "Qaqortoq" => "qaq.tg.gl",
        "Qassiarsuk" => "qsk.tg.gl",
        "Narsarsuaq" => "nrs.tg.gl",
        "Igaliku" => "iga.tg.gl",
        "Paamiut" => "paa.tg.gl",
        "Maniitsoq" => "man.tg.gl",
        "Kangerlussuaq" => "kan.tg.gl",
        "Sisimiut" => "sis.tg.gl",
        "Aasiaat" => "aas.tg.gl",
        "Qeqertarsuaq" => "qeq.tg.gl",
        "Ilulissat" => "ilu.tg.gl",
        "Qasigiannguit" => "qas.tg.gl",
        "Upernavik" => "upv.tg.gl",
        "Uummannaq" =>"uum.tg.gl",
        "Alaska" =>"ala.tg.gl"
    }.        
    
serviceprofile(Id) ->
    Profiles = econfig:get_value(em, "service_profiles"),
    proplists:get_value(Id, Profiles).
    