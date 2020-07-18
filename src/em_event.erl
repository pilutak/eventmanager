%% Copyright 2017 <thomas.elsgaard@middleaware.dk>
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

-module(em_event).

-export([process/3]).
-include("../include/em.hrl").


%%%===================================================================
%%% API
%%%===================================================================
process(Id, "GroupAutoAttendantAddInstanceRequest20", Message) ->
    em_service:create(Id, Message);
process(Id, "GroupAutoAttendantModifyInstanceRequest20", Message) ->
    em_service:modify(Id, Message);
process(Id, "GroupAutoAttendantDeleteInstanceRequest", Message) ->
    em_service:delete(Id, Message);
process(Id, "GroupHuntGroupAddInstanceRequest20", Message) ->
    em_service:create(Id, Message);
process(Id, "GroupHuntGroupModifyInstanceRequest", Message) ->
    em_service:modify(Id, Message);
process(Id, "GroupHuntGroupDeleteInstanceRequest", Message) ->
    em_service:delete(Id, Message);
process(Id, "GroupCallCenterAddInstanceRequest19", Message) ->
    em_service:create(Id, Message);
process(Id, "GroupCallCenterModifyInstanceRequest19", Message) ->
    em_service:modify(Id, Message);
process(Id, "GroupCallCenterDeleteInstanceRequest", Message) ->
    em_service:delete(Id, Message);
process(Id, "GroupMeetMeConferencingAddInstanceRequest19", Message) ->
    em_service:create(Id, Message);
process(Id, "GroupMeetMeConferencingModifyInstanceRequest", Message) ->
    em_service:modify(Id, Message);
process(Id, "GroupMeetMeConferencingDeleteInstanceRequest", Message) ->
    em_service:delete(Id, Message);
process(Id, "UserAddRequest17sp4", Message) ->
    em_user:create(Id, Message);
process(Id, "UserModifyRequest17sp4", Message) ->
    em_user:modify(Id, Message);
process(Id, "UserDeleteRequest", Message) ->
    em_user:delete(Id, Message);
process(Id, "UserAuthenticationModifyRequest", Message) ->
    em_user:set_password(Id, Message);
process(Id, "GroupTrunkGroupAddInstanceRequest21", Message) ->
    em_user:create_trunk(Id, Message);
process(Id, "GroupDeleteRequest", Message) ->
    em_user:delete_group(Id, Message);
process(Id, "SystemDomainAddRequest", Message) ->
    em_domain:create(Id, Message);
process(Id, "SystemDomainDeleteRequest", Message) ->
    em_domain:delete(Id, Message);
process(Id, "GroupVoiceMessagingGroupModifyVoicePortalRequest", Message) ->
    em_user:modify_voiceportal(Id, Message);
process(Id, "UserVoiceMessagingUserModifyAdvancedVoiceManagementRequest", Message) ->
    em_voicemail:modify(Id, Message);
process(Id, "UserBusyLampFieldModifyRequest", Message) ->
    em_blf:modify(Id, Message);    

% Ignoring status events
process(ignored, _CommandType, _Message) ->
    ok;
    
% If valid event, but not for processing, we call it a white event, and sets status to ignored    
process(Id, CommandType, _Message) ->
    logger:debug("Updating event to WHITE: ~p ~p", [Id, CommandType]),
    em_db:set_white_event(Id),
    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================
