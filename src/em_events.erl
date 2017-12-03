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
-include("../include/em.hrl").

%%%===================================================================
%%% API
%%%===================================================================

process("GroupAutoAttendantAddInstanceRequest20", Message, State) ->
    InsideCommand = em_utils:get_element_childs(Message),
    [ServiceUserId] = em_utils:get_elements(serviceUserId, InsideCommand),
    [GroupId] = em_utils:get_elements(groupId, InsideCommand),
    UserName = em_utils:get_element_text(ServiceUserId),
    GrpId = em_utils:get_element_text(GroupId),

    add_user(UserName, GrpId, 'virtual-user', State);
    %% TO DO: Query the DB to see if the userId exist, if not, continue, else ignore.
    %% Send request to EMA to create user, if there is publicID or phone in the event, 
    %% make sure to update DB and send request to HSS / ENUM

process("GroupAutoAttendantModifyInstanceRequest20", RepData, State) ->
    InsideCommand = em_utils:get_element_childs(RepData),
    [ServiceUserId] = em_utils:get_elements(serviceUserId, InsideCommand),
    [ServiceInstanceProfile] = em_utils:get_elements(serviceInstanceProfile, InsideCommand),
    [PhoneNumber] = em_utils:get_elements(phoneNumber, em_utils:get_element_childs(ServiceInstanceProfile)),
    [PublicUserIdentity] = em_utils:get_elements(publicUserIdentity, em_utils:get_element_childs(ServiceInstanceProfile)),
 
    UserName = em_utils:get_element_text(ServiceUserId),

    NewSipUri = em_utils:get_element_text(PublicUserIdentity),
    NewE164 = em_utils:get_element_text(PhoneNumber),

    SipUri = em_db:get_sipuri(UserName),
    E164 = em_db:get_e164(UserName),
   
    modify_e164(UserName, E164, NewE164, State),
    
    E164U = em_db:get_e164(UserName),
    modify_sipuri(UserName, SipUri, NewSipUri, E164U, State);
    
    

process("GroupAutoAttendantDeleteInstanceRequest", RepData, State) ->
    InsideCommand = em_utils:get_element_childs(RepData),
    [ServiceUserId] = em_utils:get_elements(serviceUserId, InsideCommand),
    UserName = em_utils:get_element_text(ServiceUserId),
    
    ?INFO_MSG("Event: GroupAutoAttendantDeleteInstanceRequest ~p", [UserName]),
    
    delete_user(UserName, State);
    %% TO DO: Query userId in DB, if the user has e164, 
    %% ENUM record must be deleted via EMA together with the HSS entry

process("UserAddRequest17sp4", RepData, _State) ->
    InsideCommand = em_utils:get_element_childs(RepData),
    [U] = em_utils:get_elements(userId, InsideCommand),
    [G] = em_utils:get_elements(groupId, InsideCommand),

    UserName = em_utils:get_element_text(U),
    GrpId = em_utils:get_element_text(G),
    add_user(UserName, GrpId, 'end-user', _State);


process("UserModifyRequest17sp4", RepData, _State) ->
    InsideCommand = em_utils:get_element_childs(RepData),
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
    E164 = em_db:get_e164(UserName),
    NewE164 = em_utils:get_element_text(P),
    SipUri = em_db:get_sipuri(UserName),
    NewSipUri = em_utils:get_element_text(L),

    modify_sipuri(UserName, SipUri, NewSipUri, E164, _State),
    modify_e164(UserName, E164, NewE164, _State);
    


process("UserDeleteRequest", RepData, _State) ->
    InsideCommand = em_utils:get_element_childs(RepData),
    [U] = em_utils:get_elements(userId, InsideCommand),

    UserName = em_utils:get_element_text(U),
    delete_user(UserName, _State);


process("GroupTrunkGroupAddInstanceRequest21", RepData, _State) ->
    InsideCommand = em_utils:get_element_childs(RepData),
    %[GroupId] = em_utils:get_elements(groupId, InsideCommand),

    [PilotUser] = em_utils:get_elements(pilotUser, InsideCommand),
    [UserId] = em_utils:get_elements(userId, em_utils:get_element_childs(PilotUser)),
    [Password] = em_utils:get_elements(password, em_utils:get_element_childs(PilotUser)),
    [LinePort] = em_utils:get_elements(linePort, em_utils:get_element_childs(PilotUser)),
 
    NewUserId = em_utils:get_element_text(UserId),
    _NewPassword = em_utils:get_element_text(Password),
    _NewLinePort = em_utils:get_element_text(LinePort),

    io:format("Pilot UserId: ~p~n",[NewUserId]);

process("GroupTrunkGroupModifyInstanceRequest20sp1", _RepData, _State) ->
    %% TODO: Investigate what to be done!
    %% What to do on <pilotUserId xsi:nil="true" />
    ignored;

process("GroupTrunkGroupDeleteInstanceRequest14sp4", _RepData, _State) ->
    %% TODO: Investigate what to be done!
    ignored;


process("GroupDeleteRequest", RepData, _State) ->
    InsideCommand = em_utils:get_element_childs(RepData),
    [G] = em_utils:get_elements(groupId, InsideCommand),
    GroupId = em_utils:get_element_text(G),

    Users = em_db:get_users(GroupId),

    lists:foreach(
        fun(I) ->
            delete_user(I, _State)
        end, Users);
 

process(CommandType, _RepData, _State) -> 
    CommandType.

%%%===================================================================
%%% Internal functions
%%%===================================================================

add_user(UserName, GrpId, 'end-user', _State) ->
    em_db:add_user(UserName, GrpId, 'end-user'),
    em_db:set_sipuri(UserName, "sip:" ++ UserName),
    ?INFO_MSG("User added: ~p", [UserName]);
add_user(UserName, GrpId, 'virtual-user', State) ->
    em_db:add_user(UserName, GrpId, 'virtual-user'),
    em_db:set_sipuri(UserName, UserName),
    em_interface_cai3g:create_subscriber(UserName, 'IMT_VIRTUAL', State),
    ?INFO_MSG("User added: ~p", [UserName]).

delete_user(UserName, State) ->
    em_db:delete_user(UserName),
    em_interface_cai3g:delete_subscriber(UserName, State),
    ?INFO_MSG("User deleted: ~p", [UserName]).

modify_e164(_UserName, undefined, undefined, _State) -> 
    ignored;
modify_e164(_UserName, Phone, Phone, _State) ->
    ignored;
modify_e164(UserName, undefined, NewPhone, State) -> 
    PubId = em_db:get_sipuri(UserName),
    em_db:set_e164(UserName, NewPhone),
    em_interface_cai3g:add_enum(NewPhone, PubId, State),
    em_interface_cai3g:add_tel_uri(UserName, NewPhone, PubId, State),
    ?INFO_MSG("Phone added for: ~p", [UserName]);
modify_e164(UserName, Phone, undefined, State) ->
    PubId = em_db:get_sipuri(UserName),
    em_db:set_e164(UserName, undefined),
    em_interface_cai3g:delete_tel_uri(UserName, Phone, State),
    em_interface_cai3g:delete_enum(Phone, PubId, State),
    ?INFO_MSG("Phone deleted for: ~p", [UserName]);
modify_e164(UserName, Phone, NewPhone, State) ->
    PubId = em_db:get_sipuri(UserName),
    em_db:set_e164(UserName, NewPhone),
    em_interface_cai3g:delete_enum(UserName, Phone, PubId, State),
    em_interface_cai3g:delete_tel_uri(UserName, Phone, State),
    em_interface_cai3g:add_enum(UserName, NewPhone, PubId, State),
    em_interface_cai3g:add_tel_uri(UserName, NewPhone, PubId, State),
    ?INFO_MSG("Phone modified for: ~p", [UserName]).
    
    
modify_sipuri(_UserName, PubId, PubId, _Phone, _State) ->
    ignored;
modify_sipuri(UserName, UserName, undefined, _Phone, _State) ->
    ignored;
modify_sipuri(UserName, PubId, undefined, undefined, State) ->
    em_db:set_sipuri(UserName, UserName),
    em_interface_cai3g:delete_pubid(UserName, "sip:" ++ PubId, State),
    em_interface_cai3g:delete_serviceprofile(UserName, PubId, State),
    em_interface_cai3g:add_serviceprofile(UserName, UserName, "IMT_VIRTUAL", State),
    em_interface_cai3g:add_pubid(UserName, "sip:" ++ UserName, UserName, State),   
    ?INFO_MSG("PubId modified to default for: ~p", [UserName]);
modify_sipuri(UserName, PubId, undefined, Phone, State) ->
    em_db:set_sipuri(UserName, UserName),
    em_interface_cai3g:delete_enum(Phone, PubId, State),  
    em_interface_cai3g:delete_tel_uri(UserName, Phone, State),  
    em_interface_cai3g:delete_pubid(UserName, "sip:" ++ PubId, State),
    em_interface_cai3g:delete_serviceprofile(UserName, PubId, State),
    em_interface_cai3g:add_enum(Phone, UserName, State),
    em_interface_cai3g:add_serviceprofile(UserName, UserName, "IMT_VIRTUAL", State),
    em_interface_cai3g:add_pubid(UserName, "sip:" ++ UserName, UserName, State),
    em_interface_cai3g:add_tel_uri(UserName, Phone, UserName, State),   
    ?INFO_MSG("PubId modified to default for: ~p", [UserName]);    
modify_sipuri(UserName, PubId, NewPubId, undefined, State) ->
    em_db:set_sipuri(UserName, NewPubId),    
    em_interface_cai3g:add_serviceprofile(UserName, NewPubId, "IMT_VIRTUAL", State),
    em_interface_cai3g:delete_pubid(UserName, "sip:" ++ PubId, State),
    em_interface_cai3g:add_pubid(UserName, "sip:" ++ NewPubId, NewPubId, State),
    em_interface_cai3g:delete_serviceprofile(UserName, PubId, State),
    ?INFO_MSG("PubId modified for: ~p", [UserName]);
modify_sipuri(UserName, PubId, NewPubId, Phone, State) ->
    em_db:set_sipuri(UserName, NewPubId),
    em_interface_cai3g:delete_enum(Phone, PubId, State),        
    em_interface_cai3g:delete_tel_uri(UserName, Phone, State), 
    em_interface_cai3g:add_serviceprofile(UserName, NewPubId, "IMT_VIRTUAL", State),
    em_interface_cai3g:delete_pubid(UserName, "sip:" ++ PubId, State),
    em_interface_cai3g:add_enum(Phone, NewPubId, State),    
    em_interface_cai3g:add_pubid(UserName, "sip:" ++ NewPubId, NewPubId, State),
    em_interface_cai3g:add_tel_uri(UserName, Phone, NewPubId, State),  
    em_interface_cai3g:delete_serviceprofile(UserName, PubId, State),
    ?INFO_MSG("PubId modified for: ~p", [UserName]).
    
