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

-module(em_cai3g).
-include("../include/em.hrl").

-export([
    add_ims_subscriber/1,
    add_ims_virtual_subscriber/1,
    add_ims_teluri/1,
    add_ims_pubid/1,
    delete_ims_teluri/2,
    delete_ims_subscriber/1,
    delete_ims_pubid/2,
    add_ims_serviceprofile/1,
    delete_ims_serviceprofile/2,
    add_ims_enum/1,
    delete_ims_enum/1,
    set_ims_pass/1,
    set_ims_phonecontext/1
    ]).
    
%%%===================================================================
%%% API
%%%===================================================================
add_ims_subscriber(IMSAssociation) ->
    Ns1 = "http://schemas.ericsson.com/ma/HSS/",
    MoType = "IMSAssociation@http://schemas.ericsson.com/ma/HSS/",
    MoId = maps:get(association, IMSAssociation), 
    PrivateUserId = maps:get(user, IMSAssociation),
    Password = maps:get(pass, IMSAssociation),
    IMPU = "sip:" ++ maps:get(pubid, IMSAssociation),
    Irs = maps:get(irs, IMSAssociation),
    ServiceProfileId = maps:get(pubid, IMSAssociation), 
    ConfiguredServiceProfileId = maps:get(csprofile, IMSAssociation),
    PhoneContext = maps:get(phonecontext, IMSAssociation),
    
    {'cai3:Create', [{'xmlns:hss', Ns1}], [
        {'cai3:MOType',[MoType]},
        {'cai3:MOId', [], [
            {'hss:associationId', [], [MoId]}]},
        {'cai3:MOAttributes',[], [
            {'hss:CreateIMSAssociation', [{'associationId', MoId}],[
                    {'hss:associationId',[MoId]},
                    {'hss:chargingProfId',["DefaultChargingProfile"]},
                    {'hss:isPsi',["false"]},
                    {'hss:privateUser', [{'privateUserId', PrivateUserId}],[
                            {'hss:privateUserId',[PrivateUserId]},
                            {'hss:userPassword',[Password]},
                            {'hss:allowedAuthMechanism',["DIGEST"]}]},
                    {'hss:publicData', [{'publicIdValue', IMPU}],[
                            {'hss:publicIdValue',[IMPU]},
                            {'hss:privateUserId',[PrivateUserId]},
                            {'hss:implicitRegSet',[Irs]},
                            {'hss:serviceProfileId',[ServiceProfileId]},
                            {'hss:sessionBarringInd',["false"]}]},
                    {'hss:subscriberServiceProfile', [{'serviceProfileId', ServiceProfileId}],[
                            {'hss:serviceProfileId',[ServiceProfileId]},
                            {'hss:configuredServiceProfile', 
                            [{'configuredServiceProfileId', ConfiguredServiceProfileId}],[
                                {'hss:configuredServiceProfileId',[ConfiguredServiceProfileId]}]},
                            {'hss:maxNumberSessions',["99"]},
                            {'hss:phoneContext',[PhoneContext]}]}]}]}]}.
           
    
add_ims_virtual_subscriber(IMSAssociation) ->
    Ns1 = "http://schemas.ericsson.com/ma/HSS/",
    MoType = "IMSAssociation@http://schemas.ericsson.com/ma/HSS/",
    MoId = maps:get(association, IMSAssociation), 
    PrivateUserId = maps:get(user, IMSAssociation),
    IMPU = "sip:" ++ maps:get(pubid, IMSAssociation),
    Irs = maps:get(irs, IMSAssociation),
    ServiceProfileId = maps:get(pubid, IMSAssociation), 
    ConfiguredServiceProfileId = maps:get(csprofile, IMSAssociation),
    PhoneContext = maps:get(phonecontext, IMSAssociation),
    
    {'cai3:Create', [{'xmlns:hss', Ns1}], [
        {'cai3:MOType',[MoType]},
        {'cai3:MOId', [], [
            {'hss:associationId', [], [MoId]}]},
        {'cai3:MOAttributes',[], [
            {'hss:CreateIMSAssociation', [{'associationId', MoId}],[
                    {'hss:associationId',[MoId]},
                    {'hss:chargingProfId',["DefaultChargingProfile"]},
                    {'hss:isPsi',["true"]},
                    {'hss:privateUser', [{'privateUserId', PrivateUserId}],[
                            {'hss:privateUserId',[PrivateUserId]}]},
                    {'hss:publicData', [{'publicIdValue', IMPU}],[
                            {'hss:publicIdValue',[IMPU]},
                            {'hss:privateUserId',[PrivateUserId]},
                            {'hss:implicitRegSet',[Irs]},
                            {'hss:serviceProfileId',[ServiceProfileId]},
                            {'hss:sessionBarringInd',["false"]}]},
                    {'hss:subscriberServiceProfile', [{'serviceProfileId', ServiceProfileId}],[
                            {'hss:serviceProfileId',[ServiceProfileId]},
                            {'hss:configuredServiceProfile', 
                            [{'configuredServiceProfileId', ConfiguredServiceProfileId}],[
                                {'hss:configuredServiceProfileId',[ConfiguredServiceProfileId]}]},
                            {'hss:maxNumberSessions',["99"]},
                            {'hss:phoneContext',[PhoneContext]}]}]}]}]}.


add_ims_teluri(IMSAssociation) ->
    Ns1 = "http://schemas.ericsson.com/ma/HSS/",
    MoType = "IMSAssociation@http://schemas.ericsson.com/ma/HSS/",
    MoId = maps:get(association, IMSAssociation), 
    PrivateUserId = maps:get(user, IMSAssociation),
    IMPU = "tel:+299" ++ maps:get(phone, IMSAssociation),
    Irs = maps:get(irs, IMSAssociation),
    IsDefault = maps:get(isdefault, IMSAssociation),
    ServiceProfileId = maps:get(pubid, IMSAssociation),
    
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
                                                   
delete_ims_teluri(User, Phone) ->
    Ns1 = "http://schemas.ericsson.com/ma/HSS/",
    Ns2 = "http://www.w3.org/2001/XMLSchema-instance",
    MoType = "IMSAssociation@http://schemas.ericsson.com/ma/HSS/",
    MoId = User,
    IMPU = "tel:+299" ++ Phone,
    
    {'cai3:Set', [{'xmlns:hss', Ns1},{'xmlns:xsi', Ns2}], [
        {'cai3:MOType',[MoType]},
        {'cai3:MOId', [], [
            {'hss:associationId', [], [MoId]}]},
        {'cai3:MOAttributes',[], [
            {'hss:SetIMSAssociation', [{'associationId', MoId}],[
                    {'hss:publicData', [{'publicIdValue', IMPU}, {'xsi:nil', "true"}],[]}
                    ]}]}]}.


delete_ims_subscriber(IMSAssociation) ->
    Ns1 = "http://schemas.ericsson.com/ma/HSS/",
    MoType = "IMSAssociation@http://schemas.ericsson.com/ma/HSS/",
    MoId = maps:get(association, IMSAssociation), 
    
    {'cai3:Delete', [{'xmlns:hss', Ns1}], [
        {'cai3:MOType',[MoType]},
        {'cai3:MOId', [], [
            {'hss:associationId', [], [MoId]}]}]}.
    
    
add_ims_pubid(IMSAssociation) ->
    Ns1 = "http://schemas.ericsson.com/ma/HSS/",
    MoType = "IMSAssociation@http://schemas.ericsson.com/ma/HSS/",
    MoId = maps:get(association, IMSAssociation), 
    PrivateUserId = maps:get(user, IMSAssociation),
    IMPU = "sip:" ++ maps:get(pubid, IMSAssociation),
    Irs = maps:get(irs, IMSAssociation),
    IsDefault = maps:get(isdefault, IMSAssociation),
    ServiceProfileId = maps:get(pubid, IMSAssociation),
    
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
    
     
delete_ims_pubid(AssociationId, CurrentPubId) ->
     Ns1 = "http://schemas.ericsson.com/ma/HSS/",
     Ns2 = "http://www.w3.org/2001/XMLSchema-instance",
     MoType = "IMSAssociation@http://schemas.ericsson.com/ma/HSS/",
     MoId = AssociationId,
     IMPU = "sip:" ++ CurrentPubId,
    
     {'cai3:Set', [{'xmlns:hss', Ns1},{'xmlns:xsi', Ns2}], [
         {'cai3:MOType',[MoType]},
         {'cai3:MOId', [], [
             {'hss:associationId', [], [MoId]}]},
         {'cai3:MOAttributes',[], [
             {'hss:SetIMSAssociation', [{'associationId', MoId}],[
                     {'hss:publicData', [{'publicIdValue', IMPU}, {'xsi:nil', "true"}],[]}
                     ]}]}]}.


add_ims_serviceprofile(IMSAssociation) ->
    Ns1 = "http://schemas.ericsson.com/ma/HSS/",
    MoType = "IMSAssociation@http://schemas.ericsson.com/ma/HSS/",
    MoId = maps:get(association, IMSAssociation),
    ServiceProfileId = maps:get(pubid, IMSAssociation),
    ConfiguredServiceProfileId = maps:get(csprofile, IMSAssociation),
    PhoneContext = maps:get(phonecontext, IMSAssociation),
    
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


delete_ims_serviceprofile(AssociationId, SProfile) ->
    Ns1 = "http://schemas.ericsson.com/ma/HSS/",
    Ns2 = "http://www.w3.org/2001/XMLSchema-instance",
    MoType = "IMSAssociation@http://schemas.ericsson.com/ma/HSS/",
    MoId = AssociationId,
    ServiceProfileId = SProfile,
    
    {'cai3:Set', [{'xmlns:hss', Ns1}, {'xmlns:xsi', Ns2}], [
        {'cai3:MOType',[MoType]},
        {'cai3:MOId', [], [
            {'hss:associationId', [], [MoId]}]},
        {'cai3:MOAttributes',[], [
            {'hss:SetIMSAssociation', [{'associationId', MoId}],[
                    {'hss:subscriberServiceProfile', [{'serviceProfileId', ServiceProfileId}, {'xsi:nil', "true"}],[]}
                    ]}]}]}.


add_ims_enum(IMSAssociation) ->
    Ns1 = "http://schemas.ericsson.com/ema/UserProvisioning/IPWorks/5.0/",
    MoType = "DNSSubscription@http://schemas.ericsson.com/ema/UserProvisioning/IPWorks/5.0/",
    MSISDN = "tel:299" ++ maps:get(phone, IMSAssociation),
    IMPU = "sip:" ++ maps:get(pubid, IMSAssociation),
    
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


delete_ims_enum(Phone) ->
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
    

set_ims_pass(IMSAssociation) ->
    Ns1 = "http://schemas.ericsson.com/ma/HSS/",
    MoType = "IMSAssociation@http://schemas.ericsson.com/ma/HSS/",
    MoId = maps:get(association, IMSAssociation),
    PrivateUserId = maps:get(user, IMSAssociation),
    Password = maps:get(pass, IMSAssociation),
    
    {'cai3:Set', [{'xmlns:hss', Ns1}], [
        {'cai3:MOType',[MoType]},
        {'cai3:MOId', [], [
            {'hss:associationId', [], [MoId]}]},
        {'cai3:MOAttributes',[], [
            {'hss:SetIMSAssociation', [{'associationId', MoId}],[
                    {'hss:privateUser', [{'privateUserId', PrivateUserId}],[
                            {'hss:userPassword',[Password]}]}
                            ]}]}]}.
           

set_ims_phonecontext(IMSAssociation) ->
    Ns1 = "http://schemas.ericsson.com/ma/HSS/",
    MoType = "IMSAssociation@http://schemas.ericsson.com/ma/HSS/",
    MoId = maps:get(association, IMSAssociation),
    ServiceProfileId = maps:get(pubid, IMSAssociation),
    PhoneContext = maps:get(phonecontext, IMSAssociation),
    
    {'cai3:Set', [{'xmlns:hss', Ns1}], [
        {'cai3:MOType',[MoType]},
        {'cai3:MOId', [], [
            {'hss:associationId', [], [MoId]}]},
        {'cai3:MOAttributes',[], [
            {'hss:SetIMSAssociation', [{'associationId', MoId}],[
                    {'hss:subscriberServiceProfile', [{'serviceProfileId', ServiceProfileId}],[
                            {'hss:phoneContext',[PhoneContext]}]
                            }]}]}]}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
