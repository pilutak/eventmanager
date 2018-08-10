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

-module(em_hss_teluri).
-include("../include/em.hrl").

-export([
    create/3,
    delete/3
    ]).
    
%%%===================================================================
%%% API
%%%===================================================================

create(User, Attrs, C) ->
    logger:debug("adding phone for user ~p", [User]),
    Req = set_teluri(User, Attrs),
    {Result, _} = send(C, Req),
    Result.
   
delete(User, Phone, C) ->
    logger:debug("deleting teluri for user ~p", [User]),    
    Req = delete_teluri(User, Phone),
    {Result, _} = send(C, Req),
    Result.


%%%===================================================================
%%% Internal functions
%%%===================================================================
set_teluri(User, Attrs) ->
    Ns1 = "http://schemas.ericsson.com/ma/HSS/",
    MoType = "IMSAssociation@http://schemas.ericsson.com/ma/HSS/",
    MoId = em_utils:md5_hex(User),
    PrivateUserId = User,
    PubId = maps:get(pubid, Attrs, User),
    ServiceProfileId = maps:get(sprofile, Attrs, PubId),
    Phone = maps:get(phone, Attrs),
    IMPU = "tel:+299" ++ Phone,    
    Irs = maps:get(irs, Attrs, "0"),
    IsDefault = maps:get(isdefault, Attrs, "false"),
    
    {'cai3:Set', [{'xmlns:hss', Ns1}], [
      {'cai3:MOType',[MoType]},
      {'cai3:MOId', [], [
          {'hss:associationId', [], [MoId]}]},
      {'cai3:MOAttributes',[], [
          {'hss:SetIMSAssociation', [{'associationId', MoId}],[
                  {'hss:publicData', [{'publicIdValue', IMPU}],[
                          {'hss:publicIdValue',[IMPU]},
                          {'hss:privateUserId',[PrivateUserId]},
                          {'hss:implicitRegSet',[Irs]},
                          {'hss:isDefault',[IsDefault]},
                          {'hss:serviceProfileId',[ServiceProfileId]},
                          {'hss:sessionBarringInd',["false"]}]}]}]}]}.
                                                   
delete_teluri(User, Phone) ->
    Ns1 = "http://schemas.ericsson.com/ma/HSS/",
    Ns2 = "http://www.w3.org/2001/XMLSchema-instance",
    MoType = "IMSAssociation@http://schemas.ericsson.com/ma/HSS/",
    MoId = em_utils:md5_hex(User),
    IMPU = "tel:+299" ++ Phone,
    {'cai3:Set', [{'xmlns:hss', Ns1},{'xmlns:xsi', Ns2}], [
        {'cai3:MOType',[MoType]},
        {'cai3:MOId', [], [
            {'hss:associationId', [], [MoId]}]},
        {'cai3:MOAttributes',[], [
            {'hss:SetIMSAssociation', [{'associationId', MoId}],[
                    {'hss:publicData', [{'publicIdValue', IMPU}, {'xsi:nil', "true"}],[]}
                    ]}]}]}.
                    
                    
send(C, Req) ->
    Resp = em_ema:send(C, Req),
    case Resp of
        {ok, Payload} -> {ok, Payload};
        Other -> logger:error("EMA request error: ~p ~p", [Other, Req]),
            Other
    end.