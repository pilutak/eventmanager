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
    
    case CommandType of
        ignored -> 
            ok;
        
        _ -> 
            {ok, Session} = em_hss:login(?EMA_USER, ?EMA_PASS),
            ?INFO_MSG("EMA session created: ~p", [Session]),
            processor(CommandType, Message, #ctx{session = Session}),
            em_hss:logout(Session),
            ?INFO_MSG("EMA session closed: ~p", [Session])
    end.
    
    
processor("GroupAutoAttendantAddInstanceRequest20", Message, Ctx) ->
    InsideCommand = em_utils:get_element_childs(Message),
    [ServiceUserId] = em_utils:get_elements(serviceUserId, InsideCommand),
    [GroupId] = em_utils:get_elements(groupId, InsideCommand),
    UserName = em_utils:get_element_text(ServiceUserId),
    GrpId = em_utils:get_element_text(GroupId),
    
    User = #subscriber{ user=UserName,
                        group_id=GrpId,
                        type='AA',
                        csprofile='IMT_VIRTUAL',
                        pubid=UserName,
                        sprofile=UserName },
    
    add_user(User, Ctx);
    %% TO DO: Query the DB to see if the userId exist, if not, continue, else ignore.
    %% Send request to EMA to create user, if there is publicID or phone in the event, 
    %% make sure to update DB and send request to HSS / ENUM
    
    
processor("GroupAutoAttendantModifyInstanceRequest20", Message, Ctx) ->
    InsideCommand = em_utils:get_element_childs(Message),
    [ServiceUserId] = em_utils:get_elements(serviceUserId, InsideCommand),
    [ServiceInstanceProfile] = em_utils:get_elements(serviceInstanceProfile, InsideCommand),
    [PhoneNumber] = em_utils:get_elements(phoneNumber, em_utils:get_element_childs(ServiceInstanceProfile)),
    [PublicUserIdentity] = em_utils:get_elements(publicUserIdentity, em_utils:get_element_childs(ServiceInstanceProfile)),
 
    UserName = em_utils:get_element_text(ServiceUserId),
    PublicId = em_utils:get_element_text(PublicUserIdentity),
    
    PubId = fix_undefined(UserName, PublicId), % If publicId contains undefined, we replace it with UserName
    Phone = em_utils:get_element_text(PhoneNumber),
    
    User = #subscriber{ user = UserName,
                        pubid = PubId,
                        phone = Phone,
                        type = 'AA', 
                        csprofile = 'IMT_VIRTUAL' 
                         },
    
    modify_user(User, Ctx);
        
processor("GroupAutoAttendantDeleteInstanceRequest", Message, Ctx) ->
    InsideCommand = em_utils:get_element_childs(Message),
    [ServiceUserId] = em_utils:get_elements(serviceUserId, InsideCommand),
    UserName = em_utils:get_element_text(ServiceUserId),
    
    delete_user(UserName, Ctx);


processor("GroupHuntGroupAddInstanceRequest20", Message, Ctx) ->
    InsideCommand = em_utils:get_element_childs(Message),
    [ServiceUserId] = em_utils:get_elements(serviceUserId, InsideCommand),
    [GroupId] = em_utils:get_elements(groupId, InsideCommand),
    UserName = em_utils:get_element_text(ServiceUserId),
    GrpId = em_utils:get_element_text(GroupId),
    
    User = #subscriber{ user=UserName,
                        group_id=GrpId,
                        type='HG',
                        csprofile='IMT_VIRTUAL',
                        pubid=UserName,
                        sprofile=UserName },
    
    add_user(User, Ctx);
    %% TO DO: Query the DB to see if the userId exist, if not, continue, else ignore.
    %% Send request to EMA to create user, if there is publicID or phone in the event, 
    %% make sure to update DB and send request to HSS / ENUM
    
processor("GroupHuntGroupModifyInstanceRequest", Message, Ctx) ->
    InsideCommand = em_utils:get_element_childs(Message),
    [ServiceUserId] = em_utils:get_elements(serviceUserId, InsideCommand),
    [ServiceInstanceProfile] = em_utils:get_elements(serviceInstanceProfile, InsideCommand),
    [PhoneNumber] = em_utils:get_elements(phoneNumber, em_utils:get_element_childs(ServiceInstanceProfile)),
    [PublicUserIdentity] = em_utils:get_elements(publicUserIdentity, em_utils:get_element_childs(ServiceInstanceProfile)),
 
    UserName = em_utils:get_element_text(ServiceUserId),
    PublicId = em_utils:get_element_text(PublicUserIdentity),
    
    PubId = fix_undefined(UserName, PublicId), % If publicId contains undefined, we replace it with UserName
    Phone = em_utils:get_element_text(PhoneNumber),
    
    User = #subscriber{ user = UserName,
                        pubid = PubId,
                        phone = Phone,
                        type = 'HG', 
                        csprofile = 'IMT_VIRTUAL' 
                         },
    
    modify_user(User, Ctx);
    
processor("GroupHuntGroupDeleteInstanceRequest", Message, Ctx) ->
    InsideCommand = em_utils:get_element_childs(Message),
    [ServiceUserId] = em_utils:get_elements(serviceUserId, InsideCommand),
    UserName = em_utils:get_element_text(ServiceUserId),
    
    delete_user(UserName, Ctx);

processor("GroupCallCenterAddInstanceRequest19", Message, Ctx) ->
    InsideCommand = em_utils:get_element_childs(Message),
    [ServiceUserId] = em_utils:get_elements(serviceUserId, InsideCommand),
    [GroupId] = em_utils:get_elements(groupId, InsideCommand),
    UserName = em_utils:get_element_text(ServiceUserId),
    GrpId = em_utils:get_element_text(GroupId),
    
    User = #subscriber{ user=UserName,
                        group_id=GrpId,
                        type='ACD',
                        csprofile='IMT_VIRTUAL',
                        pubid=UserName,
                        sprofile=UserName },
    
    add_user(User, Ctx);
    %% TO DO: Query the DB to see if the userId exist, if not, continue, else ignore.
    %% Send request to EMA to create user, if there is publicID or phone in the event, 
    %% make sure to update DB and send request to HSS / ENUM
    
processor("GroupCallCenterModifyInstanceRequest19", Message, Ctx) ->
    InsideCommand = em_utils:get_element_childs(Message),
    [ServiceUserId] = em_utils:get_elements(serviceUserId, InsideCommand),
    [ServiceInstanceProfile] = em_utils:get_elements(serviceInstanceProfile, InsideCommand),
    [PhoneNumber] = em_utils:get_elements(phoneNumber, em_utils:get_element_childs(ServiceInstanceProfile)),
    [PublicUserIdentity] = em_utils:get_elements(publicUserIdentity, em_utils:get_element_childs(ServiceInstanceProfile)),
 
    UserName = em_utils:get_element_text(ServiceUserId),
    PublicId = em_utils:get_element_text(PublicUserIdentity),
    
    PubId = fix_undefined(UserName, PublicId), % If publicId contains undefined, we replace it with UserName
    Phone = em_utils:get_element_text(PhoneNumber),
    
    User = #subscriber{ user = UserName,
                        pubid = PubId,
                        phone = Phone,
                        type = 'ACD', 
                        csprofile = 'IMT_VIRTUAL' 
                         },
    
    modify_user(User, Ctx);
    
processor("GroupCallCenterDeleteInstanceRequest", Message, Ctx) ->
    InsideCommand = em_utils:get_element_childs(Message),
    [ServiceUserId] = em_utils:get_elements(serviceUserId, InsideCommand),
    UserName = em_utils:get_element_text(ServiceUserId),
    
    delete_user(UserName, Ctx);
     
processor("GroupMeetMeConferencingAddInstanceRequest19", Message, Ctx) ->
    InsideCommand = em_utils:get_element_childs(Message),
    [ServiceUserId] = em_utils:get_elements(serviceUserId, InsideCommand),
    [GroupId] = em_utils:get_elements(groupId, InsideCommand),
    UserName = em_utils:get_element_text(ServiceUserId),
    GrpId = em_utils:get_element_text(GroupId),
    
    User = #subscriber{ user=UserName,
                        group_id=GrpId,
                        type='MEETME',
                        csprofile='IMT_VIRTUAL',
                        pubid=UserName,
                        sprofile=UserName },
    
    add_user(User, Ctx);
    %% TO DO: Query the DB to see if the userId exist, if not, continue, else ignore.
    %% Send request to EMA to create user, if there is publicID or phone in the event, 
    %% make sure to update DB and send request to HSS / ENUM
    
processor("GroupMeetMeConferencingModifyInstanceRequest", Message, Ctx) ->
    InsideCommand = em_utils:get_element_childs(Message),
    [ServiceUserId] = em_utils:get_elements(serviceUserId, InsideCommand),
    [ServiceInstanceProfile] = em_utils:get_elements(serviceInstanceProfile, InsideCommand),
    [PhoneNumber] = em_utils:get_elements(phoneNumber, em_utils:get_element_childs(ServiceInstanceProfile)),
    [PublicUserIdentity] = em_utils:get_elements(publicUserIdentity, em_utils:get_element_childs(ServiceInstanceProfile)),
 
    UserName = em_utils:get_element_text(ServiceUserId),
    PublicId = em_utils:get_element_text(PublicUserIdentity),
    
    PubId = fix_undefined(UserName, PublicId), % If publicId contains undefined, we replace it with UserName
    Phone = em_utils:get_element_text(PhoneNumber),
    
    User = #subscriber{ user = UserName,
                        pubid = PubId,
                        phone = Phone,
                        type = 'MEETME', 
                        csprofile = 'IMT_VIRTUAL' 
                         },
    
    modify_user(User, Ctx);
    
processor("GroupMeetMeConferencingDeleteInstanceRequest", Message, Ctx) ->
    InsideCommand = em_utils:get_element_childs(Message),
    [ServiceUserId] = em_utils:get_elements(serviceUserId, InsideCommand),
    UserName = em_utils:get_element_text(ServiceUserId),
    
    delete_user(UserName, Ctx);
    
processor("GroupVoiceMessagingGroupModifyVoicePortalRequest", _Message, _Ctx) ->
    %% TODO: Investigate what to be done!
    ignored;

processor("UserVoiceMessagingUserModifyAdvancedVoiceManagementRequest", _Message, _Ctx) ->
    %% TODO: Investigate what to be done!
    ignored;

processor("UserAddRequest17sp4", Message, Ctx) ->
    InsideCommand = em_utils:get_element_childs(Message),
    [U] = em_utils:get_elements(userId, InsideCommand),
    [G] = em_utils:get_elements(groupId, InsideCommand),

    UserName = em_utils:get_element_text(U),
    GrpId = em_utils:get_element_text(G),
    
    User = #subscriber{ user=UserName,
                        group_id=GrpId,
                        type='USER',
                        csprofile='IMT_CENTREX',
                        pubid=UserName,
                        sprofile=UserName },
    
    add_user(User, Ctx);


processor("UserModifyRequest17sp4", Message, Ctx) ->
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
    PubId = em_utils:get_element_text(L),


    User = #subscriber{ user = UserName,
                        pubid = PubId,
                        phone = Phone,
                        type = 'AA', 
                        csprofile = 'IMT_VIRTUAL' 
                         },
    
    modify_user(User, Ctx);


    %modify_sipuri(UserName, SipUri, NewSipUri, E164, _State),
    %modify_e164(UserName, E164, NewE164, _State);
    
processor("UserDeleteRequest", Message, Ctx) ->
    InsideCommand = em_utils:get_element_childs(Message),
    [U] = em_utils:get_elements(userId, InsideCommand),

    UserName = em_utils:get_element_text(U),
    delete_user(UserName, Ctx);


processor("UserAuthenticationModifyRequest", _Message, _Ctx) ->
    %% TODO: Investigate what to be done!
    ignored;


processor("GroupTrunkGroupAddInstanceRequest21", Message, Ctx) ->
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
    
    User = #subscriber{ user=UserName,
                        pass=SipPass,
                        group_id=GrpId,
                        type='TRUNK',
                        csprofile='BusinessTrunk',
                        pubid=PubId,
                        sprofile=PubId },
    
    add_user(User, Ctx);
    

processor("GroupTrunkGroupModifyInstanceRequest20sp1", _Message, _Ctx) ->
    %% TODO: Investigate what to be done!
    %% What to do on <pilotUserId xsi:nil="true" />
    ignored;

processor("GroupTrunkGroupDeleteInstanceRequest14sp4", _Message, _Ctx) ->
    %% TODO: Investigate what to be done!
    ignored;


processor("GroupDeleteRequest", Message, Ctx) ->
    InsideCommand = em_utils:get_element_childs(Message),
    [G] = em_utils:get_elements(groupId, InsideCommand),
    GroupId = em_utils:get_element_text(G),

    Users = em_db:get_users(GroupId),

    lists:foreach(
        fun(I) ->
            delete_user(I, Ctx)
        end, Users);


processor(ignored, _Message, _Ctx) ->
    ok;

processor(CommandType, _Message, _Ctx) ->
    ok = io:format("Received ~p ~n", [CommandType]).


%%%===================================================================
%%% Internal functions
%%%===================================================================

add_user(#subscriber{user=UserName, pass=Pass, csprofile=CSProfile, sprofile = SProfile, group_id = GrpId, type=UserType, pubid=PubId}, Ctx) ->
    em_srd:add_user(UserName, GrpId, UserType, PubId),
    em_hss:create({subscriber, UserName, Pass, CSProfile, SProfile}, Ctx),
    ?INFO_MSG("User created ~p", [UserName]).
    
delete_user(UserName, Ctx) ->
    CurrentPubId = em_srd:get_sipuri(UserName),
    CurrentPhone = em_srd:get_e164(UserName),
    
    case CurrentPhone of
        undefined ->
            em_hss:delete({subscriber, UserName}, Ctx),
            em_srd:delete_user(UserName),
            ?INFO_MSG("User deleted without ENUM ~p", [UserName]);
        CurrentPhone ->
            em_hss:delete({enum, CurrentPhone, CurrentPubId}, Ctx),
            em_hss:delete({subscriber, UserName}, Ctx),
            em_srd:delete_user(UserName),
            ?INFO_MSG("User deleted with ENUM ~p", [UserName])
    end.


modify_user(User=#subscriber{user=UserName, phone=NewPhone, csprofile=CSProfile, pubid=NewPubId}, Ctx) ->
    CurrentPubId = em_srd:get_sipuri(UserName),
    CurrentPhone = em_srd:get_e164(UserName),
    
    PlannedChange = plan_change(User),
    ?INFO_MSG("Executing plan: ~p", [PlannedChange]),
    
    case PlannedChange of
        
        {none, none} ->
            ignore;
        {none, delete} ->
            em_hss:delete({enum, CurrentPhone, CurrentPubId}, Ctx),
            em_hss:delete({teluri, UserName, CurrentPhone}, Ctx),
            em_srd:delete_e164(UserName),
            ok;       
        {none, update} ->
            em_hss:delete({enum, CurrentPhone, CurrentPubId}, Ctx),
            em_hss:delete({teluri, UserName, CurrentPhone}, Ctx),
            em_hss:create({teluri, UserName, NewPhone, CurrentPubId}, Ctx),
            em_hss:create({enum, NewPhone, UserName}, Ctx),
            em_srd:set_e164(UserName, NewPhone),
            ok;
        {none, create} ->
            em_hss:create({teluri, UserName, NewPhone, CurrentPubId}, Ctx),
            em_hss:create({enum, NewPhone, CurrentPubId}, Ctx),
            em_srd:set_e164(UserName, NewPhone),
            ok;
        {update, none} when CurrentPhone =/= undefined -> 
            em_hss:delete({enum, CurrentPhone, CurrentPubId}, Ctx),
            em_hss:delete({teluri, UserName, CurrentPhone}, Ctx),
            em_hss:delete({pubid, UserName, CurrentPubId}, Ctx),
            em_hss:delete({serviceprofile, UserName, CurrentPubId}, Ctx),
            em_hss:create({serviceprofile, UserName, NewPubId, CSProfile}, Ctx),
            em_hss:create({pubid, UserName, NewPubId, NewPubId}, Ctx),
            em_hss:create({teluri, UserName, CurrentPhone, NewPubId}, Ctx),
            em_hss:create({enum, NewPhone, NewPubId}, Ctx),
            em_srd:set_sipuri(UserName, NewPubId),
            ok;
        {update, none} -> 
            em_hss:delete({pubid, UserName, CurrentPubId}, Ctx),
            em_hss:delete({serviceprofile, UserName, CurrentPubId}, Ctx),
            em_hss:create({serviceprofile, UserName, NewPubId, CSProfile}, Ctx),
            em_hss:create({pubid, UserName, NewPubId, NewPubId}, Ctx),
            em_srd:set_sipuri(UserName, NewPubId),
            ok;
        {update, update} ->
            em_hss:delete({enum, CurrentPhone, CurrentPubId}, Ctx),
            em_hss:delete({teluri, UserName, CurrentPhone}, Ctx),
            em_hss:delete({pubid, UserName, CurrentPubId}, Ctx),
            em_hss:delete({serviceprofile, UserName, CurrentPubId}, Ctx),
            em_hss:create({serviceprofile, UserName, NewPubId, CSProfile}, Ctx),
            em_hss:create({pubid, UserName, NewPubId, NewPubId}, Ctx),
            em_hss:create({teluri, UserName, NewPhone, NewPubId}, Ctx),
            em_hss:create({enum, NewPhone, NewPubId}, Ctx),
            em_srd:set_sipuri(UserName, NewPubId),
            em_srd:set_e164(UserName, NewPhone),
            ok;
        {update, create} ->
            em_hss:delete({pubid, UserName, CurrentPubId}, Ctx),
            em_hss:delete({serviceprofile, UserName, CurrentPubId}, Ctx),
            em_hss:create({serviceprofile, UserName, NewPubId, CSProfile}, Ctx),
            em_hss:create({pubid, UserName, NewPubId, NewPubId}, Ctx),
            em_hss:create({teluri, UserName, NewPhone, NewPubId}, Ctx),
            em_hss:create({enum, NewPhone, NewPubId}, Ctx),
            em_srd:set_sipuri(UserName, NewPubId),
            em_srd:set_e164(UserName, NewPhone),
            ok;
        {update, delete} ->
            em_hss:delete({enum, CurrentPhone, CurrentPubId}, Ctx),
            em_hss:delete({teluri, UserName, CurrentPhone}, Ctx),
            em_hss:delete({pubid, UserName, CurrentPubId}, Ctx),
            em_hss:delete({serviceprofile, UserName, CurrentPubId}, Ctx),
            em_hss:create({serviceprofile, UserName, NewPubId, CSProfile}, Ctx),
            em_hss:create({pubid, UserName, NewPubId, NewPubId}, Ctx),
            em_srd:set_sipuri(UserName, NewPubId),
            em_srd:delete_e164(UserName),
            ok;
        {ActionPubId, ActionPhone} ->
            ?ERROR_MSG("Unknown planned change: ~p", [{ActionPubId, ActionPhone}])
            
    end.
    
% This planning of steps is creted in order to limit he numbers of entries in the case statement
plan_change(#subscriber{user=UserName, pubid=NewPubId, phone=NewPhone}) ->
    CurrentPubId = em_srd:get_sipuri(UserName),
    CurrentPhone = em_srd:get_e164(UserName),  
    PubIdAction = plan_change(CurrentPubId, NewPubId),
    PhoneAction = plan_change(CurrentPhone, NewPhone),
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

% This fix is made because we are adding a default pubud to users, before 
% the pubId is added via CommPilot
fix_undefined(UserName, undefined) ->
    UserName;
fix_undefined(_UserName, PubId) ->
    PubId.
     

         
        
