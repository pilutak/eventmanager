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

-module(em_hss_serviceprofile).
-include("../include/em.hrl").

-export([
    create/3,
    delete/3,
    phonecontext/3
    ]).
    
%%%===================================================================
%%% API
%%%===================================================================

create(User, Attrs, C) ->
    logger:debug("adding serviceprofile for user ~p", [User]),
    Req = set_serviceprofile(User, Attrs),
    {Result, _} = send(C, Req),
    Result.
   
delete(User, ServiceProfileId, C) ->
    logger:debug("deleting serviceprofile ~p", [ServiceProfileId]),    
    Req = delete_serviceprofile(User, ServiceProfileId),
    {Result, _} = send(C, Req),
    Result.

phonecontext(User, Attrs, C) ->
    logger:debug("Updating phonecontext for user ~p", [User]),    
    Req = set_phonecontext(User, Attrs),
    {Result, _} = send(C, Req),
    Result.



%%%===================================================================
%%% Internal functions
%%%===================================================================
set_serviceprofile(User, Attrs) ->
    Ns1 = "http://schemas.ericsson.com/ma/HSS/",
    MoType = "IMSAssociation@http://schemas.ericsson.com/ma/HSS/",
    MoId = em_utils:md5_hex(User), 
    PubId = maps:get(pubid, Attrs, User),   
    ServiceProfileId = maps:get(sprofile, Attrs, PubId),
    Type = maps:get(type, Attrs),    
    ConfiguredServiceProfileId = maps:get(csprofile, Attrs, em_utils:serviceprofile(Type)),
    PhoneContext = maps:get(phonecontext, Attrs, "tg.gl"),    
    
    {'cai3:Set', [{'xmlns:hss', Ns1}], [
        {'cai3:MOType',[MoType]},
        {'cai3:MOId', [], [
            {'hss:associationId', [], [MoId]}]},
        {'cai3:MOAttributes',[], [
            {'hss:SetIMSAssociation', [{'associationId', MoId}],[
                    {'hss:subscriberServiceProfile', [{'serviceProfileId', ServiceProfileId}],[
                            {'hss:serviceProfileId',[ServiceProfileId]},
                            {'hss:configuredServiceProfile', 
                            [{'configuredServiceProfileId', ConfiguredServiceProfileId}],[
                                {'hss:configuredServiceProfileId',[ConfiguredServiceProfileId]}]},
                            {'hss:maxNumberSessions',["99"]},
                            {'hss:phoneContext',[PhoneContext]}]}]}]}]}.

set_phonecontext(User, Attrs) ->
    Ns1 = "http://schemas.ericsson.com/ma/HSS/",
    MoType = "IMSAssociation@http://schemas.ericsson.com/ma/HSS/",
    MoId = em_utils:md5_hex(User),
    PubId = maps:get(pubid, Attrs, User),      
    ServiceProfileId = maps:get(sprofile, Attrs, PubId),
    PhoneContext = maps:get(phonecontext, Attrs, "tg.gl"),    
    {'cai3:Set', [{'xmlns:hss', Ns1}], [
        {'cai3:MOType',[MoType]},
        {'cai3:MOId', [], [
            {'hss:associationId', [], [MoId]}]},
        {'cai3:MOAttributes',[], [
            {'hss:SetIMSAssociation', [{'associationId', MoId}],[
                    {'hss:subscriberServiceProfile', [{'serviceProfileId', ServiceProfileId}],[
                            {'hss:phoneContext',[PhoneContext]}]
                            }]}]}]}.

delete_serviceprofile(User, ServiceProfileId) ->
    Ns1 = "http://schemas.ericsson.com/ma/HSS/",
    Ns2 = "http://www.w3.org/2001/XMLSchema-instance",
    MoType = "IMSAssociation@http://schemas.ericsson.com/ma/HSS/",
    MoId = em_utils:md5_hex(User),     
    {'cai3:Set', [{'xmlns:hss', Ns1}, {'xmlns:xsi', Ns2}], [
        {'cai3:MOType',[MoType]},
        {'cai3:MOId', [], [
            {'hss:associationId', [], [MoId]}]},
        {'cai3:MOAttributes',[], [
            {'hss:SetIMSAssociation', [{'associationId', MoId}],[
                    {'hss:subscriberServiceProfile', [{'serviceProfileId', ServiceProfileId}, {'xsi:nil', "true"}],[]}
                    ]}]}]}.
    
send(C, Req) ->
    logger:debug("EMA Req: ~p",[Req]),
    Resp = em_ema:send(C, Req),
    case Resp of
        {ok, Payload} -> {ok, Payload};
        Other -> logger:error("EMA request error: ~p ~p", [Other, Req]),
            Other
    end.