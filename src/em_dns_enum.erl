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

-module(em_dns_enum).
-include("../include/em.hrl").

-export([
    create/3,
    delete/3
    ]).
    
%%%===================================================================
%%% API
%%%===================================================================

create(User, Attrs, C) ->
    logger:debug("Creating ENUM for user ~p", [User]),
    Req = create_enum(User, Attrs),
    {Result, _} = send(C, Req),
    Result.
   
delete(User, Phone, C) ->
    logger:debug("Deleting ENUM for user ~p", [User]),    
    Req = delete_enum(User, Phone),
    {Result, _} = send(C, Req),
    Result.


%%%===================================================================
%%% Internal functions
%%%===================================================================
create_enum(User, Attrs) ->
    Ns1 = "http://schemas.ericsson.com/ema/UserProvisioning/IPWorks/5.0/",
    MoType = "DNSSubscription@http://schemas.ericsson.com/ema/UserProvisioning/IPWorks/5.0/",
    Phone = maps:get(phone, Attrs),
    PubId = maps:get(pubid, Attrs, User),
    MSISDN = "tel:299" ++ Phone,
    IMPU = "sip:" ++ PubId,
    
    {'cai3:Create', [{'xmlns:ns', Ns1}], [
        {'cai3:MOType',[MoType]},
        {'cai3:MOId', [], [
            {'ns:msisdn', [], [MSISDN]}]},
        {'cai3:MOAttributes',[], [
            {'ns:createDNSSubscription', [{'msisdn', MSISDN}],[
                    {'ns:msisdn',[MSISDN]},        
                    {'ns:records', [{'publicId', IMPU}],[
                            {'ns:publicId',[IMPU]},
                            {'ns:flags',["nu"]},
                            {'ns:order',["10"]},
                            {'ns:preference',["10"]},
                            {'ns:service',["E2U+sip"]}]}]}]}]}.


delete_enum(_User, Phone) ->
    Ns1 = "http://schemas.ericsson.com/ema/UserProvisioning/IPWorks/5.0/",
    MoType = "DNSSubscription@http://schemas.ericsson.com/ema/UserProvisioning/IPWorks/5.0/",
    MSISDN = "tel:299" ++ Phone,
    {'cai3:Delete', [{'xmlns:ns', Ns1}], [
        {'cai3:MOType',[MoType]},
        {'cai3:MOId', [], [
            {'ns:msisdn', [], [MSISDN]}]},
        {'cai3:MOAttributes',[], [
            {'ns:deleteDNSSubscription', [{'msisdn', MSISDN}],[]}
            ]}]}.
    
send(C, Req) ->
    logger:debug("EMA Req: ~p",[Req]),
    Resp = em_ema:send(C, Req),
    case Resp of
        {ok, Payload} -> {ok, Payload};
        Other -> logger:error("EMA request error: ~p ~p", [Other, Req]),
            Other
    end.