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

-export([process/2]).
-include("../include/em.hrl").


%%%===================================================================
%%% API
%%%===================================================================
process(CommandType, Message) ->
    Processor = maps:get(CommandType, processors(), ignored),
    processor(Processor, Message).

%%%===================================================================
%%% Internal functions
%%%===================================================================
processor(create_service, Message) ->
    InsideCommand = em_utils:get_element_childs(Message),
    [ServiceUserId] = em_utils:get_elements(serviceUserId, InsideCommand),
    [GroupId] = em_utils:get_elements(groupId, InsideCommand),
    UserName = em_utils:get_element_text(ServiceUserId),
    GrpId = em_utils:get_element_text(GroupId), 
    Event=#event{user=UserName, pubid=UserName, group=GrpId, sprofile=UserName},
    em_processor_service:create(type_is_virtual(Event));

processor(modify_service, Message) ->
    InsideCommand = em_utils:get_element_childs(Message),
    [ServiceUserId] = em_utils:get_elements(serviceUserId, InsideCommand),
    [ServiceInstanceProfile] = em_utils:get_elements(serviceInstanceProfile, InsideCommand),
    [PhoneNumber] = em_utils:get_elements(phoneNumber, em_utils:get_element_childs(ServiceInstanceProfile)),
    [PublicUserIdentity] = em_utils:get_elements(publicUserIdentity, em_utils:get_element_childs(ServiceInstanceProfile)),
    UserName = em_utils:get_element_text(ServiceUserId),
    PublicId = em_utils:get_element_text(PublicUserIdentity),
    Phone = em_utils:get_element_text(PhoneNumber),
    PubId_is_nil = em_utils:get_element_attributes('xsi:nil',PublicUserIdentity) =:= "true",
    Phone_is_nil = em_utils:get_element_attributes('xsi:nil',PhoneNumber) =:= "true",
    Pub = nil_fix(PublicId, PubId_is_nil), 
    Pho = nil_fix(Phone, Phone_is_nil),        
    Event=#event{user=UserName, pubid=Pub, phone=Pho, sprofile=Pub, current_pubid=em_srd:get_sipuri(UserName), current_phone=em_srd:get_e164(UserName)},
    em_processor_service:modify(type_is_virtual(Event));
    
processor(modify_group_vp, Message) ->
    InsideCommand = em_utils:get_element_childs(Message),
    [GroupId] = em_utils:get_elements(groupId, InsideCommand),
    [ServiceInstanceProfile] = em_utils:get_elements(serviceInstanceProfile, InsideCommand),
    [P] = em_utils:get_elements(phoneNumber, em_utils:get_element_childs(ServiceInstanceProfile)),
    [Pub] = em_utils:get_elements(publicUserIdentity, em_utils:get_element_childs(ServiceInstanceProfile)),
    Phone = em_utils:get_element_text(P),
    PubId = em_utils:get_element_text(Pub),
    GrpId = em_utils:get_element_text(GroupId),
    UserName = PubId,
    Event=#event{user=PubId, pubid=PubId, phone=Phone, group=GrpId},  
    
    case em_srd:user_exists(UserName) of
        false ->
            em_processor_service:create(type_is_virtual(Event)),
            em_processor_service:modify(type_is_virtual(Event));
        true ->
            em_processor_service:modify(Event)
    end;

processor(modify_user_vm, Message) ->
    InsideCommand = em_utils:get_element_childs(Message),
    [U] = em_utils:get_elements(userId, InsideCommand),
    [G] = em_utils:get_elements(groupMailServerUserId, InsideCommand),
    [P] = em_utils:get_elements(groupMailServerPassword, InsideCommand),
    _UserName = em_utils:get_element_text(U),
    _MailUser = em_utils:get_element_text(G),
    _MailPass = em_utils:get_element_text(P);
        
    %TODO Create data in surgemail

processor(create_user, Message) ->
    InsideCommand = em_utils:get_element_childs(Message),
    [U] = em_utils:get_elements(userId, InsideCommand),
    [G] = em_utils:get_elements(groupId, InsideCommand),
    UserName = em_utils:get_element_text(U),
    GrpId = em_utils:get_element_text(G),
    Event=#event{user=UserName, pubid=UserName, group=GrpId, sprofile=UserName},
    em_processor_user:create(type_is_user(Event));
    

processor(modify_user, Message) ->
    InsideCommand = em_utils:get_element_childs(Message),
    [U] = em_utils:get_elements(userId, InsideCommand),
    [P] = em_utils:get_elements(phoneNumber, InsideCommand),

    % Fetch endpoint data
    [E] = em_utils:get_elements(endpoint, InsideCommand),
    [A] = em_utils:get_elements(accessDeviceEndpoint, em_utils:get_element_childs(E)),
    [L] = em_utils:get_elements(linePort, em_utils:get_element_childs(A)),

    %% Fetch address/state (used for 112/113 emergency routing)
    [AD] = em_utils:get_elements(address, InsideCommand),
    [SP] = em_utils:get_elements(stateOrProvince, em_utils:get_element_childs(AD)),
    City = em_utils:get_element_text(SP),
    PhoneContext = maps:get(City, phonecontexts(), undefined),
    
    case PhoneContext of
        undefined ->
            ok;
        PhoneContext ->
            io:format("PhoneContext is: ~p~n",[PhoneContext])
    end,
    
    
    % Fecth endpoint Trunk data
    [F] = em_utils:get_elements(endpoint, InsideCommand),
    [T] = em_utils:get_elements(trunkAddressing, em_utils:get_element_childs(F)),
    [TG] = em_utils:get_elements(trunkGroupDeviceEndpoint, em_utils:get_element_childs(T)),
    [LP] = em_utils:get_elements(linePort, em_utils:get_element_childs(TG)),
    TrunkLinePort = em_utils:get_element_text(LP),
    io:format("Trunk LinePort: ~p~n",[TrunkLinePort]),
    UserName = em_utils:get_element_text(U),
    Phone = em_utils:get_element_text(P),
    PublicId = em_utils:get_element_text(L),
    PubId = fix_undefined(UserName, PublicId), % If publicId contains undefined, we replace it with UserName
   
    PubId_is_nil = em_utils:get_element_attributes('xsi:nil',L) =:= "true",
    Phone_is_nil = em_utils:get_element_attributes('xsi:nil',P) =:= "true",
   
    Pub = nil_fix(PubId, PubId_is_nil),        
    Pho = nil_fix(Phone, Phone_is_nil),
   
    Event=#event{
                user = UserName, 
                pubid = Pub, 
                phone = Pho, 
                group=em_srd:get_group(UserName), 
                sprofile=Pub, 
                current_pubid=em_srd:get_sipuri(UserName), 
                current_phone=em_srd:get_e164(UserName)
                },
                
    case TrunkLinePort of
        undefined ->
            em_processor_user:modify(type_is_user(Event));
        TrunkLinePort ->
            em_processor_trunk:modify(type_is_trunk(Event))
    end;


processor(delete_service, Message) ->
    InsideCommand = em_utils:get_element_childs(Message),
    [ServiceUserId] = em_utils:get_elements(serviceUserId, InsideCommand),
    UserName = em_utils:get_element_text(ServiceUserId),
    em_processor_service:delete(#event{user=UserName});

processor(delete_user, Message) ->
    InsideCommand = em_utils:get_element_childs(Message),
    [U] = em_utils:get_elements(userId, InsideCommand),
    UserName = em_utils:get_element_text(U),
    em_processor_user:delete(#event{user=UserName});

processor(set_password, Message) ->
    InsideCommand = em_utils:get_element_childs(Message),
    [U] = em_utils:get_elements(userId, InsideCommand),
    [P] = em_utils:get_elements(newPassword, InsideCommand),
    UserName = em_utils:get_element_text(U),
    Pass = em_utils:get_element_text(P),
    set_user_password(UserName, Pass);

processor(create_trunk, Message) ->
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
    Event=#event{user=UserName, pass=SipPass, pubid=PubId, group=GrpId},
    em_processor_trunk:create(type_is_pilot(Event));

%processor("GroupTrunkGroupModifyInstanceRequest20sp1", Message, _Ctx) ->
%    InsideCommand = em_utils:get_element_childs(Message),
%    [GroupId] = em_utils:get_elements(groupId, InsideCommand),
%    [SipPassWord] = em_utils:get_elements(sipAuthenticationPassword, InsideCommand),

%    [PilotUser] = em_utils:get_elements(pilotUser, InsideCommand),
%    [PilotUserId] = em_utils:get_elements(userId, em_utils:get_element_childs(PilotUser)),
%    [LinePort] = em_utils:get_elements(linePort, em_utils:get_element_childs(PilotUser)),
 
%    UserName = em_utils:get_element_text(PilotUserId),
%    SipPass = em_utils:get_element_text(SipPassWord),
%    PubId = em_utils:get_element_text(LinePort),
%    GrpId = em_utils:get_element_text(GroupId),
    
%    io:format(UserName),
%    io:format(SipPass),
%    io:format(PubId),
%    io:format(GrpId);

    %TODO find a way to get the pilot user ID, and update the password of the user. We might not need this event
    % since the password update can be done via the SIP authentication modify on the pilot user.

processor(delete_group, Message) ->
    InsideCommand = em_utils:get_element_childs(Message),
    [G] = em_utils:get_elements(groupId, InsideCommand),
    GroupId = em_utils:get_element_text(G),
    Users = em_db:get_users(GroupId),

    lists:foreach(
        fun(I) ->
            em_processor_user:delete(I)
        end, Users);


processor(ignored, _Message) ->
    ok.
    
set_user_password(UserName, Pass) ->
    State=#state{session=open_ema_session()},
    em_hss:update({pass, UserName, Pass}, State),
    em_srd:set_pass(UserName, Pass),
    close_ema_session(State).

type_is_virtual(Event) ->
    Event#event{type='virtual',
                  csprofile='IMT_VIRTUAL',
                  ispsi='TRUE',
                  isdefault='FALSE',
                  irs='0',
                  pass=randchar(14)}.

type_is_trunk(Event) ->
    Event#event{type='trunk',
                  csprofile='BusinessTrunk_wild',
                  ispsi='TRUE',
                  isdefault='FALSE',
                  irs='0',
                  pass=randchar(14)}.

type_is_user(Event) ->
    Event#event{type='user',
                  csprofile='IMS_CENTREX',
                  ispsi='FALSE',
                  isdefault='TRUE',
                  irs='1',
                  pass=randchar(14)}.      

type_is_pilot(Event) ->
    Event#event{type='pilot',
                  csprofile='BusinessTrunk',
                  ispsi='FALSE',
                  isdefault='TRUE',
                  irs='1'}.  

nil_fix(undefined, false) ->
    undefined;
nil_fix(undefined, true) ->
    nil;
nil_fix(X, _) ->
    X.


open_ema_session() ->
    {ok, Session} = em_hss:login(?EMA_USER, ?EMA_PASS),
    ?INFO_MSG("EMA session created: ~p", [Session]),
    Session.
    
close_ema_session(#state{session=Session}) ->
    em_hss:logout(Session),
    ?INFO_MSG("EMA session closed: ~p", [Session]).    
    

% We use this to create a temporarily SIP password. The password is later
% overwritten by an seperate event (not for virtual users, the password remains).
randchar(N) ->
   randchar(N, []).
   
randchar(0, Acc) ->
   Acc;
randchar(N, Acc) ->
   randchar(N - 1, [rand:uniform(26) + 96 | Acc]).      
   
   
% This fix is made because we are adding a default pubud to users, before 
% the pubId is added via CommPilot
fix_undefined(UserName, undefined) ->
    UserName;
fix_undefined(_UserName, PubId) ->
    PubId.
        

processors() ->
    #{  
        "GroupAutoAttendantAddInstanceRequest20" => create_service,
        "GroupAutoAttendantModifyInstanceRequest20" => modify_service,
        "GroupAutoAttendantDeleteInstanceRequest" => delete_service,
        "GroupHuntGroupAddInstanceRequest20" => create_service,
        "GroupHuntGroupModifyInstanceRequest" => modify_service,
        "GroupHuntGroupDeleteInstanceRequest" => delete_service,
        "GroupCallCenterAddInstanceRequest19" => create_service,
        "GroupCallCenterModifyInstanceRequest" => modify_service,
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
        "GroupDeleteRequest" => delete_group
    }.

phonecontexts() ->
    #{
        "Nuuk" => 'nuk.tg.gl',
        "Nanortalik" => 'nan.tg.gl',
        "Narsaq" => 'nar.tg.gl',
        "Qaqortoq" => 'qaq.tg.gl',
        "Qassiarsuk" => 'qsk.tg.gl',
        "Narsarsuaq" => 'nrs.tg.gl',
        "Igaliku" => 'iga.tg.gl',
        "Paamiut" => 'paa.tg.gl',
        "Maniitsoq" => 'man.tg.gl',
        "Kangerlussuaq" => 'kan.tg.gl',
        "Sisimiut" => 'sis.tg.gl',
        "Aasiaat" => 'aas.tg.gl',
        "Qeqertarsuaq" => 'qeq.tg.gl',
        "Ilulissat" => 'ilu.tg.gl',
        "Qasigiannguit" => 'qas.tg.gl',
        "Upernavik" => 'upv.tg.gl',
        "Uummannaq" =>'uum.tg.gl',
        "Alaska" =>'alaska.tg.gl'
    }.
    

        