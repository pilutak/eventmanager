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
    Event=#event{user=UserName, pass=randchar(14), pubid=UserName, group=GrpId, ispsi='TRUE', irs='0', isdefault='FALSE', csprofile='IMT_VIRTUAL'},
    
    add_user(Event);

processor(modify_virtual_user, Message) ->
    InsideCommand = em_utils:get_element_childs(Message),
    [ServiceUserId] = em_utils:get_elements(serviceUserId, InsideCommand),
    [ServiceInstanceProfile] = em_utils:get_elements(serviceInstanceProfile, InsideCommand),
    [PhoneNumber] = em_utils:get_elements(phoneNumber, em_utils:get_element_childs(ServiceInstanceProfile)),
    [PublicUserIdentity] = em_utils:get_elements(publicUserIdentity, em_utils:get_element_childs(ServiceInstanceProfile)),
    UserName = em_utils:get_element_text(ServiceUserId),
    PublicId = em_utils:get_element_text(PublicUserIdentity),
    PubId = fix_undefined(UserName, PublicId), % If publicId contains undefined, we replace it with UserName
    Phone = em_utils:get_element_text(PhoneNumber),
    Event=#event{user=UserName, pubid=PubId, phone=Phone, irs='0', isdefault='FALSE', csprofile='IMT_VIRTUAL', sprofile=PubId},
    modify_user(Event);


processor(modify_group_vp, Message) ->
    InsideCommand = em_utils:get_element_childs(Message),
    [GroupId] = em_utils:get_elements(groupId, InsideCommand),
    [ServiceInstanceProfile] = em_utils:get_elements(serviceInstanceProfile, InsideCommand),
    [P] = em_utils:get_elements(phoneNumber, em_utils:get_element_childs(ServiceInstanceProfile)),
    [Pub] = em_utils:get_elements(publicUserIdentity, em_utils:get_element_childs(ServiceInstanceProfile)),
    Phone = em_utils:get_element_text(P),
    UserName = em_utils:get_element_text(Pub),
    GrpId = em_utils:get_element_text(GroupId),

    io:format(UserName),
    io:format(GrpId),
    io:format(Phone);
    
    %TODO Create data in IMS core


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
    
    %TODO Create data in IMS core

processor(create_user, Message) ->
    InsideCommand = em_utils:get_element_childs(Message),
    [U] = em_utils:get_elements(userId, InsideCommand),
    [G] = em_utils:get_elements(groupId, InsideCommand),
    UserName = em_utils:get_element_text(U),
    GrpId = em_utils:get_element_text(G),

    Event=#event{user=UserName, pass=randchar(14), pubid=UserName, group=GrpId, ispsi='FALSE', irs='1', isdefault='TRUE', csprofile='IMS_CENTREX'},
    add_user(Event);
    

processor(modify_user, Message) ->
  InsideCommand = em_utils:get_element_childs(Message),
   [U] = em_utils:get_elements(userId, InsideCommand),
   [P] = em_utils:get_elements(phoneNumber, InsideCommand),

   % Fetch endpoint data
   [E] = em_utils:get_elements(endpoint, InsideCommand),
   [A] = em_utils:get_elements(accessDeviceEndpoint, em_utils:get_element_childs(E)),
   [L] = em_utils:get_elements(linePort, em_utils:get_element_childs(A)),

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

   case TrunkLinePort of
       undefined ->
           Event = #event{ 
               user = UserName,
               pubid = PubId,
               phone = Phone,
               csprofile='IMS_CENTREX',
               ispsi='FALSE',
               irs='1',
               isdefault='TRUE'
               },
               modify_user(Event);
                   
       TrunkLinePort ->
           Event = #event{ 
               user = UserName,
               pubid = PubId,
               phone = Phone, 
               csprofile='BusinessTrunk_wild',
               ispsi='TRUE',
               irs='0',
               isdefault='FALSE' 
               },
               delete_user(UserName),
               add_user(Event),
               modify_user(Event)
               %TODO We must not delete the user without keeping tack of the group ID
               % We might need a different way to do this.
   end;
    

processor(delete_virtual_user, Message) ->
    InsideCommand = em_utils:get_element_childs(Message),
    [ServiceUserId] = em_utils:get_elements(serviceUserId, InsideCommand),
    UserName = em_utils:get_element_text(ServiceUserId),
    delete_user(UserName);
    
processor(delete_user, Message) ->
    InsideCommand = em_utils:get_element_childs(Message),
    [U] = em_utils:get_elements(userId, InsideCommand),
    UserName = em_utils:get_element_text(U),
    delete_user(UserName);

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
    Event=#event{user=UserName, pass=SipPass, pubid=PubId, group=GrpId, ispsi='FALSE', irs='1', isdefault='TRUE', csprofile='BusinessTrunk'},
    add_user(Event);

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
            delete_user(I)
        end, Users);


processor(ignored, _Message) ->
    ok.

add_user(Event) ->
    State=#state{session=open_ema_session()},    
    {ok, _} = em_srd:add_user(Event),
    em_hss:create({subscriber, Event}, State),
    close_ema_session(State).

delete_user(UserName) ->
    CurrentPubId = em_srd:get_sipuri(UserName),
    CurrentPhone = em_srd:get_e164(UserName),

    State=#state{session=open_ema_session()},
    
        case CurrentPhone of
            undefined ->
               em_hss:delete({subscriber, UserName}, State),
               {ok, _} = em_srd:delete_user(UserName);
           
            CurrentPhone ->
                em_hss:delete({enum, CurrentPhone, CurrentPubId}, State),
                em_hss:delete({subscriber, UserName}, State),
                {ok, _} = em_srd:delete_user(UserName)
        end,
        
    close_ema_session(State).    
    

modify_user(Event=#event{user=UserName}) ->
    State=#state{session=open_ema_session()},

    CurrentPubId = em_srd:get_sipuri(UserName),
    CurrentPhone = em_srd:get_e164(UserName),
    
    PlannedChange = plan_change(Event),
    ?INFO_MSG("Executing plan: ~p", [PlannedChange]),
    
    case PlannedChange of
        
        {none, none} ->
            ignore;
        {none, delete} ->
            em_hss:delete({enum, CurrentPhone, CurrentPubId}, State),
            em_hss:delete({teluri, UserName, CurrentPhone}, State),
            em_srd:delete_e164(UserName),
            ok;       
        {none, update} ->
            em_srd:set_e164(Event),
            em_hss:delete({enum, CurrentPhone, CurrentPubId}, State),
            em_hss:delete({teluri, UserName, CurrentPhone}, State),
            em_hss:create({teluri, Event}, State),
            em_hss:create({enum, Event}, State),    
            ok;
        {none, create} ->
            em_srd:set_e164(Event),
            em_hss:create({teluri, Event}, State),
            em_hss:create({enum, Event}, State),
            ok;
        {update, none} when CurrentPhone =/= undefined ->
            em_hss:delete({enum, CurrentPhone, CurrentPubId}, State),
            em_hss:delete({teluri, UserName, CurrentPhone}, State),
            em_hss:delete({pubid, UserName, CurrentPubId}, State),
            em_hss:delete({serviceprofile, UserName, CurrentPubId}, State),
            em_srd:set_sipuri(Event), 
            em_hss:create({serviceprofile, Event}, State),
            em_hss:create({pubid, Event}, State),
            em_hss:create({teluri, Event}, State),
            em_hss:create({enum, Event}, State),
            ok;
        {update, none} ->
            em_hss:delete({pubid, UserName, CurrentPubId}, State),
            em_hss:delete({serviceprofile, UserName, CurrentPubId}, State),
            em_srd:set_sipuri(Event),
            em_hss:create({serviceprofile, Event}, State),
            em_hss:create({pubid, Event}, State),
            ok;
        {update, update} ->
            em_hss:delete({enum, CurrentPhone, CurrentPubId}, State),
            em_hss:delete({teluri, UserName, CurrentPhone}, State),
            em_hss:delete({pubid, UserName, CurrentPubId}, State),
            em_hss:delete({serviceprofile, UserName, CurrentPubId}, State),
            em_srd:set_sipuri(Event),
            em_srd:set_e164(Event),
            em_hss:create({serviceprofile, Event}, State),
            em_hss:create({pubid, Event}, State),
            em_hss:create({teluri, Event}, State),
            em_hss:create({enum, Event}, State),
            ok;
        {update, create} ->
            em_hss:delete({pubid, UserName, CurrentPubId}, State),
            em_hss:delete({serviceprofile, UserName, CurrentPubId}, State),
            em_srd:set_e164(Event),
            em_srd:set_sipuri(Event),
            em_hss:create({serviceprofile, Event}, State),
            em_hss:create({pubid, Event}, State),
            em_hss:create({teluri, Event}, State),
            em_hss:create({enum, Event}, State),
            ok;
        {update, delete} ->
            em_hss:delete({enum, CurrentPhone, CurrentPubId}, State),
            em_hss:delete({teluri, UserName, CurrentPhone}, State),
            em_srd:delete_e164(UserName),
            em_hss:delete({pubid, UserName, CurrentPubId}, State),
            em_hss:delete({serviceprofile, UserName, CurrentPubId}, State),
            em_srd:set_sipuri(Event),
            em_hss:create({serviceprofile, Event}, State),
            em_hss:create({pubid, Event}, State),
            ok;
        {ActionPubId, ActionPhone} ->
            ?ERROR_MSG("Unknown planned change: ~p", [{ActionPubId, ActionPhone}])
            
    end,
    close_ema_session(State).
    
    
set_user_password(UserName, Pass) ->
    State=#state{session=open_ema_session()},
    em_hss:update({pass, UserName, Pass}, State),
     close_ema_session(State).
    
    
% This planning of steps is creted in order to limit he numbers of entries in the case statement
plan_change(#event{user=UserName, pubid=PubId, phone=Phone}) ->
    CurrentPubId = em_srd:get_sipuri(UserName),
    CurrentPhone = em_srd:get_e164(UserName),  
    PubIdAction = plan_change(CurrentPubId, PubId),
    PhoneAction = plan_change(CurrentPhone, Phone),
    {PubIdAction, PhoneAction}.
    
plan_change(undefined, undefined) ->
    none;
plan_change(undefined, _X) ->
    create;
plan_change(_X, _X) ->
    none;
plan_change(_X, undefined) ->
    delete;
plan_change(_X, _Y) ->
    update.


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