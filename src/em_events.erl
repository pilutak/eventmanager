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
    ProcessorType = select_processor(CommandType), 
    processor(ProcessorType, Message).
    %TODO: We should only open EMA sessions in supported commands

%%%===================================================================
%%% Internal functions
%%%===================================================================
processor(create_virtual_user, Message) ->
    InsideCommand = em_utils:get_element_childs(Message),
    [ServiceUserId] = em_utils:get_elements(serviceUserId, InsideCommand),
    [GroupId] = em_utils:get_elements(groupId, InsideCommand),
    UserName = em_utils:get_element_text(ServiceUserId),
    GrpId = em_utils:get_element_text(GroupId),
    
    Event=#event{user=UserName, pubid=UserName, group=GrpId, sprofile=UserName},
    em_processor_service:create(type_is_virtual(Event));

processor(modify_virtual_user, Message) ->
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
    
    Event=#event{user=UserName, pubid=Pub, phone=Pho, sprofile=Pub},
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
    UserName = em_utils:get_element_text(U),
    MailUser = em_utils:get_element_text(G),
    MailPass = em_utils:get_element_text(P),
    
    io:format(UserName),
    io:format(MailUser),
    io:format(MailPass);
    
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

    %% Fetch address/state
    %[AD] = em_utils:get_elements(address, InsideCommand),
    %[SP] = em_utils:get_elements(stateOrProvince, em_utils:get_element_childs(AD)),
    %City = em_utils:get_element_text(SP),
    
    %PhoneContext = city_to_phonecontext(City),
    
    %case PhoneContext of
    %    undefined ->
    %        io:format("PhoneContext is undefined: ~p~n",[City]);
    %    PhoneContext ->
    %        io:format("PhoneContext is: ~p~n",[PhoneContext])
    %end,

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
   
   % In order to re-create the SIP Trunk user, we must first get the GrpId
   GrpId = em_srd:get_group(UserName),
   
   Event=#event{user = UserName, pubid = Pub, phone = Pho, group=GrpId },
   case TrunkLinePort of
       undefined ->
           em_processor_user:modify(type_is_user(Event));
       TrunkLinePort ->
           em_processor_user:modify(type_is_trunk(Event))
   end;


processor(delete_virtual_user, Message) ->
    InsideCommand = em_utils:get_element_childs(Message),
    [ServiceUserId] = em_utils:get_elements(serviceUserId, InsideCommand),
    UserName = em_utils:get_element_text(ServiceUserId),
    em_processor_service:delete(#event{user=UserName});

processor(delete_user, Message) ->
    InsideCommand = em_utils:get_element_childs(Message),
    [U] = em_utils:get_elements(userId, InsideCommand),
    UserName = em_utils:get_element_text(U),
    em_processor_user:delete(UserName);

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
    em_processor_user:create(type_is_pilot(Event));

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
        

select_processor("GroupAutoAttendantAddInstanceRequest20") ->
    create_virtual_user;
select_processor("GroupAutoAttendantModifyInstanceRequest20") ->
    modify_virtual_user;
select_processor("GroupAutoAttendantDeleteInstanceRequest") ->
    delete_virtual_user;
select_processor("GroupHuntGroupAddInstanceRequest20") ->
    create_virtual_user;
select_processor("GroupHuntGroupModifyInstanceRequest") ->
    modify_virtual_user;
select_processor("GroupHuntGroupDeleteInstanceRequest") ->
    delete_virtual_user;
select_processor("GroupCallCenterAddInstanceRequest19") ->
    create_virtual_user;
select_processor("GroupCallCenterModifyInstance") ->
    modify_virtual_user;
select_processor("GroupCallCenterDeleteInstanceRequest") ->
    delete_virtual_user;
select_processor("GroupMeetMeConferencingAddInstanceRequest19") ->
    create_virtual_user;
select_processor("GroupMeetMeConferencingModifyInstanceRequest") ->
    modify_virtual_user;
select_processor("GroupMeetMeConferencingDeleteInstanceRequest") ->
    delete_virtual_user;
select_processor("UserAddRequest17sp4") ->
    create_user;
select_processor("UserModifyRequest17sp4") ->
    modify_user;
select_processor("UserDeleteRequest") ->
    delete_user;
select_processor("UserAuthenticationModifyRequest") ->
    set_password;
select_processor("GroupTrunkGroupAddInstanceRequest21") ->
    create_trunk;
select_processor("GroupVoiceMessagingGroupModifyVoicePortalRequest") ->
    modify_group_vp;
select_processor("UserVoiceMessagingUserModifyAdvancedVoiceManagementRequest") ->
    modify_user_vm;
select_processor("GroupDeleteRequest") ->
    delete_group;
select_processor(_) ->
    ignored.
    
    
    
%city_to_phonecontext('Nuuk') ->
%    'nuk.tg.gl';  
%city_to_phonecontext('Nanortalik') ->
%    'nan.tg.gl'; 
%city_to_phonecontext('Narsaq') ->
%    'nar.tg.gl';
%city_to_phonecontext('Qaqortoq') ->
%    'qaq.tg.gl';
%city_to_phonecontext('Qassiarsuk') ->
%    'qsk.tg.gl';
%city_to_phonecontext('Narsarsuaq') ->
%    'nrs.tg.gl';
%city_to_phonecontext('Igaliku') ->
%    'iga.tg.gl';
%city_to_phonecontext('Paamiut') ->
%    'paa.tg.gl';
%city_to_phonecontext('Maniitsoq') ->
%    'man.tg.gl';
%city_to_phonecontext('Kangerlussuaq') ->
%    'kan.tg.gl';
%city_to_phonecontext('Sisimiut') ->
%    'sis.tg.gl';
%city_to_phonecontext('Aasiaat') ->
%    'aas.tg.gl';
%city_to_phonecontext('Qeqertarsuaq') ->
%    'qeq.tg.gl';
%city_to_phonecontext('Ilulissat') ->
%    'ilu.tg.gl';
%city_to_phonecontext('Qasigiannguit') ->
%    'qas.tg.gl';
%city_to_phonecontext('Upernavik') ->
%    'upv.tg.gl';
%city_to_phonecontext('Uummannaq') ->
%    'uum.tg.gl';
%city_to_phonecontext(City) ->
%    City.
        