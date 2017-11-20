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

%% API
-export([process/3]).
-include("../include/em.hrl").


process("GroupAutoAttendantAddInstanceRequest20", RepData, _State) ->
    InsideCommand = em_utils:get_element_childs(RepData),
    [ServiceUserId] = em_utils:get_elements(serviceUserId, InsideCommand),
    UserName = em_utils:get_element_text(ServiceUserId),

    add_user(UserName, 'virtual-user', _State);
    %% TO DO: Query the DB to see if the userId exist, if not, continue, else ignore.
    %% Send request to EMA to create user, if there is publicID or phone in the event, 
    %% make sure to update DB and send request to HSS / ENUM

    %em_utils:log(gen_server:call(em_interface_cai3g,{create_subscriber,em_utils:get_element_text(ServiceUserId)}));

process("GroupAutoAttendantModifyInstanceRequest20", RepData, _State) ->
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

    modify_e164(E164, NewE164, UserName, _State),
    modify_sipuri(SipUri, NewSipUri, UserName, _State);

    
    %[ServiceInstanceProfile] = em_utils:get_elements(serviceInstanceProfile,Inside_command),
    %[PhoneNumber] = em_utils:get_elements(phoneNumber,em_utils:get_element_childs(ServiceInstanceProfile)),
    %[PublicUserIdentity] = em_utils:get_elements(publicUserIdentity,em_utils:get_element_childs(ServiceInstanceProfile)),
    %Have_PhoneNumber = em_utils:get_element_attributes('xsi:nil',PhoneNumber) =/= "true",
    %Have_PublicUserIdentity = em_utils:get_element_attributes('xsi:nil',PublicUserIdentity) =/= "true",
    %add_telURI(Have_PhoneNumber,em_utils:get_element_text(ServiceUserId),em_utils:get_element_text(PhoneNumber),State),
    %add_sipURI(Have_PublicUserIdentity,em_utils:get_element_text(ServiceUserId),em_utils:get_element_text(PublicUserIdentity),State);

process("GroupAutoAttendantDeleteInstanceRequest", RepData, _State) ->
    InsideCommand = em_utils:get_element_childs(RepData),
    [ServiceUserId] = em_utils:get_elements(serviceUserId, InsideCommand),
    UserName = em_utils:get_element_text(ServiceUserId),
    delete_user(UserName, _State);
    %% TO DO: Query userId in DB, if the user has e164, 
    %% ENUM record must be deleted via EMA together with the HSS entry

    %[ServiceUserId]=em_utils:get_elements(serviceUserId,em_utils:get_element_childs(RepData)),
    %io:format(ServiceUserId);
    %em_utils:log(gen_server:call(em_interface_cai3g,{del_user,em_utils:get_element_text(ServiceUserId)}));


process("UserAddRequest17sp4", RepData, _State) ->
    InsideCommand = em_utils:get_element_childs(RepData),
    %[G] = em_utils:get_elements(groupId, Inside_command),
    [U] = em_utils:get_elements(userId, InsideCommand),

    %GroupId = em_utils:get_element_text(G),
    UserName = em_utils:get_element_text(U),
    add_user(UserName, 'end-user', _State);


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

    modify_e164(E164, NewE164, UserName, _State),
    modify_sipuri(SipUri, NewSipUri, UserName, _State);


process("UserDeleteRequest", RepData, _State) ->
    InsideCommand = em_utils:get_element_childs(RepData),
    %[G] = em_utils:get_elements(groupId, Inside_command),
    [U] = em_utils:get_elements(userId, InsideCommand),

    %GroupId = em_utils:get_element_text(G),
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


process(_OtherThing, _RepData, _State) -> 
    ignored.


%add_telURI(false,_,_,_)->ignored;
    %add_telURI(true,ServiceUserId,PhoneNumber,_State)->
    %em_utils:log(gen_server:call(em_interface_cai3g,{add_telURI,ServiceUserId,PhoneNumber})).


%add_sipURI(false,_,_,_)->ignored;
%add_sipURI(true,ServiceUserId,PublicUserIdentity,_State)->
%em_utils:log(gen_server:call(em_interface_cai3g,{add_sipURI,ServiceUserId,PublicUserIdentity})).

add_user(UserName, Type, _State) ->
    em_db:add_user(UserName, Type),
    em_utils:log("User added to SRD"),
    em_utils:log("HSS Subscriber created").

delete_user(UserName, _State) ->
    em_db:delete_user(UserName),
    em_utils:log("User deleted from SRD"),
    em_utils:log("HSS Subscriber deleted"),
    em_utils:log("ENUM record deleted").

modify_e164(undefined, undefined, _, _) -> 
    ignored;
modify_e164(E164, E164, _, _State) ->
    ignored;
modify_e164(undefined, NewE164, UserName, _) -> 
    em_db:set_e164(UserName, NewE164);
modify_e164(_E164, undefined, UserName, _State) ->
    em_db:set_e164(UserName, undefined).

modify_sipuri(undefined, undefined, _, _) -> 
    ignored;
modify_sipuri(SipUri, SipUri, _, _State) ->
    ignored;
modify_sipuri(undefined, NewSipUri, UserName, _) -> 
    em_db:set_sipuri(UserName, NewSipUri);
modify_sipuri(_SipUri, undefined, UserName, _State) ->
    em_db:set_sipuri(UserName, undefined).
