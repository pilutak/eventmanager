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
    Inside_command=em_utils:get_element_childs(RepData),
    [ServiceUserId] = em_utils:get_elements(serviceUserId, Inside_command),
    UserName = em_utils:get_element_text(ServiceUserId),
    em_db:add_user(UserName, virtual);

    %% TO DO: Query the DB to see if the userId exist, if not, continue, else ignore.
    %% Send request to EMA to create user, if there is publicID or phone in the event, 
    %% make sure to update DB and send request to HSS / ENUM

    %em_utils:log(gen_server:call(em_interface_cai3g,{create_subscriber,em_utils:get_element_text(ServiceUserId)}));

process("GroupAutoAttendantModifyInstanceRequest20", RepData, _State) ->
    Inside_command=em_utils:get_element_childs(RepData),
    [ServiceUserId] = em_utils:get_elements(serviceUserId, Inside_command),
    [ServiceInstanceProfile] = em_utils:get_elements(serviceInstanceProfile, Inside_command),
    [PhoneNumber] = em_utils:get_elements(phoneNumber, em_utils:get_element_childs(ServiceInstanceProfile)),
    [PublicUserIdentity] = em_utils:get_elements(publicUserIdentity, em_utils:get_element_childs(ServiceInstanceProfile)),
 
    UserName = em_utils:get_element_text(ServiceUserId),
    SipUri = em_utils:get_element_text(PublicUserIdentity),
    E164 = em_utils:get_element_text(PhoneNumber),

    modify_e164(E164, UserName, _State),
    modify_sipuri(SipUri, UserName, _State);

    
    %[ServiceInstanceProfile] = em_utils:get_elements(serviceInstanceProfile,Inside_command),
    %[PhoneNumber] = em_utils:get_elements(phoneNumber,em_utils:get_element_childs(ServiceInstanceProfile)),
    %[PublicUserIdentity] = em_utils:get_elements(publicUserIdentity,em_utils:get_element_childs(ServiceInstanceProfile)),
    %Have_PhoneNumber = em_utils:get_element_attributes('xsi:nil',PhoneNumber) =/= "true",
    %Have_PublicUserIdentity = em_utils:get_element_attributes('xsi:nil',PublicUserIdentity) =/= "true",
    %add_telURI(Have_PhoneNumber,em_utils:get_element_text(ServiceUserId),em_utils:get_element_text(PhoneNumber),State),
    %add_sipURI(Have_PublicUserIdentity,em_utils:get_element_text(ServiceUserId),em_utils:get_element_text(PublicUserIdentity),State);

process("GroupAutoAttendantDeleteInstanceRequest", RepData, _State) ->
    Inside_command=em_utils:get_element_childs(RepData),
    [ServiceUserId] = em_utils:get_elements(serviceUserId,Inside_command),
    UserId = em_utils:get_element_text(ServiceUserId),
    em_db:delete_user(UserId);

    %% TO DO: Query userId in DB, if the user has e164, 
    %% ENUM record must be deleted via EMA together with the HSS entry

    %[ServiceUserId]=em_utils:get_elements(serviceUserId,em_utils:get_element_childs(RepData)),
    %io:format(ServiceUserId);
    %em_utils:log(gen_server:call(em_interface_cai3g,{del_user,em_utils:get_element_text(ServiceUserId)}));



process(_OtherThing, _RepData, _State) -> 
    ignored.


%add_telURI(false,_,_,_)->ignored;
    %add_telURI(true,ServiceUserId,PhoneNumber,_State)->
    %em_utils:log(gen_server:call(em_interface_cai3g,{add_telURI,ServiceUserId,PhoneNumber})).


%add_sipURI(false,_,_,_)->ignored;
%add_sipURI(true,ServiceUserId,PublicUserIdentity,_State)->
%em_utils:log(gen_server:call(em_interface_cai3g,{add_sipURI,ServiceUserId,PublicUserIdentity})).

modify_e164(undefined, _, _) -> 
    % TODO: if e164 exist in SRD, delete e164 from HSS subscriber and ENUM, else ignore.
    ignored;
modify_e164(E164, UserName, _State) ->
    % TODO: if e164 not exist in SRD, add e164 to HSS subscriber and ENUM, else ignore.  
    io:format("e164 added: ~p~n ~p~n", [E164, UserName]).

modify_sipuri(undefined, _, _) -> 
    % TODO: if SipUri exist in SRD, delete SipUri from HSS subscriber, else ignore.
    ignored;
modify_sipuri(SipUri, UserName, _State) ->
    % TODO: if SipUri not exist in SRD, add SipUri to HSS subscriber, else ignore.   
    io:format("SipUri added: ~p~n ~p~n", [SipUri, UserName]).
