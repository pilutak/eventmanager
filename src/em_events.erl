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
            processor(CommandType, Message, #ctx{session = Session}),
            em_hss:logout(Session)
            
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
    PubId = em_utils:get_element_text(PublicUserIdentity),
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


processor("GroupTrunkGroupAddInstanceRequest21", Message, _Ctx) ->
    InsideCommand = em_utils:get_element_childs(Message),
    %[GroupId] = em_utils:get_elements(groupId, InsideCommand),

    [PilotUser] = em_utils:get_elements(pilotUser, InsideCommand),
    [UserId] = em_utils:get_elements(userId, em_utils:get_element_childs(PilotUser)),
    [Password] = em_utils:get_elements(password, em_utils:get_element_childs(PilotUser)),
    [LinePort] = em_utils:get_elements(linePort, em_utils:get_element_childs(PilotUser)),
 
    NewUserId = em_utils:get_element_text(UserId),
    _NewPassword = em_utils:get_element_text(Password),
    _NewLinePort = em_utils:get_element_text(LinePort),

    io:format("Pilot UserId: ~p~n",[NewUserId]);

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

add_user(#subscriber{user=UserName, csprofile=CSProfile, sprofile = SProfile, group_id = GrpId, type=UserType, pubid=PubId}, Ctx) ->
    em_srd:add_user(UserName, GrpId, UserType, PubId),
    em_hss:create({subscriber, UserName, CSProfile, SProfile}, Ctx),
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
    case get_action(User) of
        
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
            em_hss:create({teluri, UserName, NewPhone, UserName}, Ctx),
            em_hss:create({enum, NewPhone, UserName}, Ctx),
            em_srd:set_e164(UserName, NewPhone),
            ok;
        {none, create} ->
            em_hss:create({teluri, UserName, NewPhone, UserName}, Ctx),
            em_hss:create({enum, NewPhone, UserName}, Ctx),
            em_srd:set_e164(UserName, NewPhone),
            ok;
        {delete, none} ->
            em_hss:delete({pubid, UserName, CurrentPubId}, Ctx),
            em_hss:delete({serviceprofile, UserName, CurrentPubId}, Ctx),
            em_hss:create({serviceprofile, UserName, UserName, CSProfile}, Ctx),
            em_hss:create({pubid, UserName, UserName, UserName}, Ctx),
            em_srd:set_sipuri_default(UserName),
            ok;       
        {delete, delete} ->
            em_hss:delete({enum, CurrentPhone, CurrentPubId}, Ctx),
            em_hss:delete({teluri, UserName, CurrentPhone}, Ctx),
            em_hss:delete({pubid, UserName, CurrentPubId}, Ctx),
            em_hss:delete({serviceprofile, UserName, CurrentPubId}, Ctx),
            em_hss:create({serviceprofile, UserName, UserName, CSProfile}, Ctx),
            em_hss:create({pubid, UserName, UserName, UserName}, Ctx),
            em_srd:set_sipuri_default(UserName),
            ok;
        {delete, update} ->
            em_hss:delete({enum, CurrentPhone, CurrentPubId}, Ctx),
            em_hss:delete({teluri, UserName, CurrentPhone}, Ctx),
            em_hss:delete({pubid, UserName, CurrentPubId}, Ctx),
            em_hss:delete({serviceprofile, UserName, CurrentPubId}, Ctx),
            em_hss:create({serviceprofile, UserName, UserName, CSProfile}, Ctx),
            em_hss:create({pubid, UserName, UserName, UserName}, Ctx),
            em_hss:create({teluri, UserName, NewPhone, UserName}, Ctx),
            em_hss:create({enum, NewPhone, UserName}, Ctx),
            em_srd:set_sipuri_default(UserName),
            em_srd:set_e164(UserName, NewPhone),
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
            em_hss:create({teluri, UserName, NewPhone, UserName}, Ctx),
            em_hss:create({enum, NewPhone, UserName}, Ctx),
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
            ?ERROR_MSG("Unknown action plan: ~p", [{ActionPubId, ActionPhone}])
            
    end.
    

get_action(#subscriber{user=UserName, pubid=NewPubId, phone=NewPhone}) ->
    CurrentPubId = em_srd:get_sipuri(UserName),
    CurrentPhone = em_srd:get_e164(UserName),
    
    PubIdAction = get_action(CurrentPubId, NewPubId),
    PhoneAction = get_action(CurrentPhone, NewPhone),
    {PubIdAction, PhoneAction}.
    
get_action(undefined, undefined) ->
    none;
get_action(undefined, _X) ->
    create;
get_action(_X, _X) ->
    none;
get_action(_X, undefined) ->
    delete;
get_action(_X, _Y) ->
    update.

    
