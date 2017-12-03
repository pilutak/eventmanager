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

-module(em_interface_cai3g_envelopes).

-export([
    login/2,
    logout/1,
    add_subscriber/3,
    add_tel_uri/4,
    add_pubid/4,
    delete_tel_uri/3,
    delete_subscriber/2,
    delete_pubid/3,
    add_serviceprofile/4,
    delete_serviceprofile/3
    ]).
    
%%%===================================================================
%%% API
%%%===================================================================

login(User, Pass) ->
    io_lib:format(
    "<soapenv:Envelope xmlns:soapenv=\"http://schemas.xmlsoap.org/soap/envelope/\" xmlns:cai3=\"http://schemas.ericsson.com/cai3g1.2/\">
        <soapenv:Header/>
        <soapenv:Body>
            <cai3:Login>
              <cai3:userId>~s</cai3:userId>
              <cai3:pwd>~s</cai3:pwd>
            </cai3:Login>
        </soapenv:Body>
    </soapenv:Envelope>",[User, Pass]).

logout(Session) ->
    io_lib:format(
    "<soapenv:Envelope xmlns:soapenv=\"http://schemas.xmlsoap.org/soap/envelope/\" xmlns:cai3=\"http://schemas.ericsson.com/cai3g1.2/\">
        <soapenv:Header>
            <cai3:SessionId>~s</cai3:SessionId>
        </soapenv:Header>
        <soapenv:Body>
            <cai3:Logout>
                <cai3:sessionId>~s</cai3:sessionId>
            </cai3:Logout>
        </soapenv:Body>
    </soapenv:Envelope>",[Session, Session]).

add_subscriber(Session, User, Profile) ->
    io_lib:format(
    "<soapenv:Envelope xmlns:soapenv=\"http://schemas.xmlsoap.org/soap/envelope/\" xmlns:cai3=\"http://schemas.ericsson.com/cai3g1.2/\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\">
        <soapenv:Header>
            <cai3:SessionId>~s</cai3:SessionId>
        </soapenv:Header>
        <soapenv:Body>
            <cai3:Create>
                <cai3:MOType>ISMSubscription@http://schemas.ericsson.com/ema/UserProvisioning/HSS/ISM/</cai3:MOType>
                <cai3:MOId>
                    <subscriberId>~s</subscriberId>
                </cai3:MOId>
                <cai3:MOAttributes>
                    <CreateISMSubscription xmlns=\"http://schemas.ericsson.com/ema/UserProvisioning/HSS/ISM/\" subscriberId=\"~s\">
                        <subscriberId>~s</subscriberId>
                        <chargingProfId>DefaultChargingProfile</chargingProfId>
                        <isPsi>TRUE</isPsi>
                        <privateUser privateUserId=\"~s\">
                            <privateUserId>~s</privateUserId>
                            <userPassword>123456</userPassword>
                            <allowedAuthMechanism>Digest</allowedAuthMechanism>
                            </privateUser>
                        <publicData publicIdValue=\"sip:~s\">
                            <publicIdValue>sip:~s</publicIdValue>
                            <privateUserId>~s</privateUserId>
                            <serviceProfileId>~s</serviceProfileId> 
                            <implicitRegSet>0</implicitRegSet>
                        </publicData>
                        <subscriberServiceProfile serviceProfileId=\"~s\">
                            <serviceProfileId>~s</serviceProfileId>
                            <configuredServiceProfile configuredServiceProfileId=\"~s\">
                                <configuredServiceProfileId>~s</configuredServiceProfileId>
                            </configuredServiceProfile>
                            <maxNumberSessions>99</maxNumberSessions>
                        </subscriberServiceProfile>
                    </CreateISMSubscription>
                </cai3:MOAttributes>
            </cai3:Create>
        </soapenv:Body>
    </soapenv:Envelope>",[Session, User, User, User, User, User, User, User, User, User, User, User, Profile, Profile]).

add_tel_uri(Session, User, E164, SipUri) ->
    io_lib:format(
    "<soapenv:Envelope xmlns:soapenv=\"http://schemas.xmlsoap.org/soap/envelope/\" xmlns:cai3=\"http://schemas.ericsson.com/cai3g1.2/\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\">
        <soapenv:Header>
            <cai3:SessionId>~s</cai3:SessionId>
        </soapenv:Header>
        <soapenv:Body>
            <cai3:Set>
                <cai3:MOType>ISMSubscription@http://schemas.ericsson.com/ema/UserProvisioning/HSS/ISM/</cai3:MOType>
                <cai3:MOId>
                    <subscriberId>~s</subscriberId>
                </cai3:MOId>
                <cai3:MOAttributes>
                    <SetISMSubscription xmlns=\"http://schemas.ericsson.com/ema/UserProvisioning/HSS/ISM/\" subscriberId=\"~s\">
                        <publicData publicIdValue=\"tel:+~s\">
                            <publicIdValue>tel:+~s</publicIdValue>
                            <privateUserId>~s</privateUserId>
                            <implicitRegSet>0</implicitRegSet>
                            <isDefault>FALSE</isDefault>
                            <serviceProfileId>~s</serviceProfileId>
                            <sessionBarringInd>FALSE</sessionBarringInd>
                        </publicData>
                    </SetISMSubscription>
                </cai3:MOAttributes>
            </cai3:Set>
        </soapenv:Body>
    </soapenv:Envelope>",[Session, User, User, E164, E164, User, SipUri]).

delete_tel_uri(Session, User, E164) ->
    io_lib:format(
    "<soapenv:Envelope xmlns:soapenv=\"http://schemas.xmlsoap.org/soap/envelope/\" xmlns:cai3=\"http://schemas.ericsson.com/cai3g1.2/\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\">
        <soapenv:Header>
            <cai3:SessionId>~s</cai3:SessionId>
        </soapenv:Header>
        <soapenv:Body>
            <cai3:Set>
                <cai3:MOType>ISMSubscription@http://schemas.ericsson.com/ema/UserProvisioning/HSS/ISM/</cai3:MOType>
                <cai3:MOId>
                    <subscriberId>~s</subscriberId>
                </cai3:MOId>
                <cai3:MOAttributes>
                    <SetISMSubscription xmlns=\"http://schemas.ericsson.com/ema/UserProvisioning/HSS/ISM/\" subscriberId=\"~s\">
                        <publicData publicIdValue=\"tel:+~s\">
                            <publicIdState>not_registered</publicIdState>
                        </publicData>
                        <publicData publicIdValue=\"tel:+~s\" xsi:nil=\"true\"/>
                    </SetISMSubscription>
                </cai3:MOAttributes>
            </cai3:Set>
        </soapenv:Body>
    </soapenv:Envelope>",[Session, User, User, E164, E164]).

delete_subscriber(Session, User) ->
    io_lib:format(
    "<soapenv:Envelope xmlns:soapenv=\"http://schemas.xmlsoap.org/soap/envelope/\" xmlns:cai3=\"http://schemas.ericsson.com/cai3g1.2/\">
        <soapenv:Header>
            <cai3:SessionId>~s</cai3:SessionId>
        </soapenv:Header>
        <soapenv:Body>
            <cai3:Delete>
                <cai3:MOType>ISMSubscription@http://schemas.ericsson.com/ema/UserProvisioning/HSS/ISM/</cai3:MOType>
                <cai3:MOId>
                    <subscriberId>~s</subscriberId>
                </cai3:MOId>
            </cai3:Delete>
        </soapenv:Body>
    </soapenv:Envelope>",[Session, User]).
    
add_pubid(Session, User, PubIdValue, ServiceProfile) ->
    io_lib:format(
    "<soapenv:Envelope xmlns:soapenv=\"http://schemas.xmlsoap.org/soap/envelope/\" xmlns:cai3=\"http://schemas.ericsson.com/cai3g1.2/\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\">
        <soapenv:Header>
            <cai3:SessionId>~s</cai3:SessionId>
        </soapenv:Header>
        <soapenv:Body>
            <cai3:Set>
                <cai3:MOType>ISMSubscription@http://schemas.ericsson.com/ema/UserProvisioning/HSS/ISM/</cai3:MOType>
                <cai3:MOId>
                    <subscriberId>~s</subscriberId>
                </cai3:MOId>
                <cai3:MOAttributes>
                    <SetISMSubscription xmlns=\"http://schemas.ericsson.com/ema/UserProvisioning/HSS/ISM/\" subscriberId=\"~s\">
                        <publicData publicIdValue=\"~s\">
                            <publicIdValue>~s</publicIdValue>
                            <privateUserId>~s</privateUserId>
                            <implicitRegSet>0</implicitRegSet>
                            <isDefault>FALSE</isDefault>
                            <serviceProfileId>~s</serviceProfileId>
                            <sessionBarringInd>FALSE</sessionBarringInd>
                        </publicData>
                    </SetISMSubscription>
                </cai3:MOAttributes>
            </cai3:Set>
        </soapenv:Body>
    </soapenv:Envelope>",[Session, User, User, PubIdValue, PubIdValue, User, ServiceProfile]).
    
delete_pubid(Session, User, PubIdValue) ->
    io_lib:format(
    "<soapenv:Envelope xmlns:soapenv=\"http://schemas.xmlsoap.org/soap/envelope/\" xmlns:cai3=\"http://schemas.ericsson.com/cai3g1.2/\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\">
        <soapenv:Header>
            <cai3:SessionId>~s</cai3:SessionId>
        </soapenv:Header>
        <soapenv:Body>
            <cai3:Set>
                <cai3:MOType>ISMSubscription@http://schemas.ericsson.com/ema/UserProvisioning/HSS/ISM/</cai3:MOType>
                <cai3:MOId>
                    <subscriberId>~s</subscriberId>
                </cai3:MOId>
                <cai3:MOAttributes>
                    <SetISMSubscription xmlns=\"http://schemas.ericsson.com/ema/UserProvisioning/HSS/ISM/\" subscriberId=\"~s\">
                        <publicData publicIdValue=\"~s\">
                            <publicIdState>not_registered</publicIdState>
                        </publicData>
                        <publicData publicIdValue=\"~s\" xsi:nil=\"true\"/>
                    </SetISMSubscription>
                </cai3:MOAttributes>
            </cai3:Set>
        </soapenv:Body>
    </soapenv:Envelope>",[Session, User, User, PubIdValue, PubIdValue]).
    

add_serviceprofile(Session, User, ServiceProfile, ConfiguredServiceProfile) ->
    io_lib:format(
    "<soapenv:Envelope xmlns:soapenv=\"http://schemas.xmlsoap.org/soap/envelope/\" xmlns:cai3=\"http://schemas.ericsson.com/cai3g1.2/\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\">
        <soapenv:Header>
            <cai3:SessionId>~s</cai3:SessionId>
        </soapenv:Header>
        <soapenv:Body>
            <cai3:Set>
                <cai3:MOType>ISMSubscription@http://schemas.ericsson.com/ema/UserProvisioning/HSS/ISM/</cai3:MOType>
                <cai3:MOId>
                    <subscriberId>~s</subscriberId>
                </cai3:MOId>
                <cai3:MOAttributes>
                    <SetISMSubscription xmlns=\"http://schemas.ericsson.com/ema/UserProvisioning/HSS/ISM/\" subscriberId=\"~s\">
		                <subscriberServiceProfile serviceProfileId=\"~s\"> 
                            <serviceProfileId>~s</serviceProfileId> 
                            <configuredServiceProfile configuredServiceProfileId=\"~s\"> 
                                <configuredServiceProfileId>~s\</configuredServiceProfileId> 
                            </configuredServiceProfile>
                            <subscribedMediaProfile>99</subscribedMediaProfile>
                            <maxNumberSessions>99</maxNumberSessions>
                            <phoneContext>tg.gl</phoneContext>
                        </subscriberServiceProfile>
                    </SetISMSubscription>
                </cai3:MOAttributes>
            </cai3:Set>
        </soapenv:Body>
    </soapenv:Envelope>",[Session, User, User, ServiceProfile, ServiceProfile, ConfiguredServiceProfile, ConfiguredServiceProfile]).
        
delete_serviceprofile(Session, User, ServiceProfile) ->
    io_lib:format(
    "<soapenv:Envelope xmlns:soapenv=\"http://schemas.xmlsoap.org/soap/envelope/\" xmlns:cai3=\"http://schemas.ericsson.com/cai3g1.2/\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\">
        <soapenv:Header>
            <cai3:SessionId>~s</cai3:SessionId>
        </soapenv:Header>
        <soapenv:Body>
            <cai3:Set>
                <cai3:MOType>ISMSubscription@http://schemas.ericsson.com/ema/UserProvisioning/HSS/ISM/</cai3:MOType>
                <cai3:MOId>
                    <subscriberId>~s</subscriberId>
                </cai3:MOId>
                <cai3:MOAttributes>
                    <SetISMSubscription xmlns=\"http://schemas.ericsson.com/ema/UserProvisioning/HSS/ISM/\" subscriberId=\"~s\">
                        <subscriberServiceProfile serviceProfileId=\"~s\" xsi:nil=\"true\">
                        </subscriberServiceProfile> 
                    </SetISMSubscription>
                </cai3:MOAttributes>
            </cai3:Set>
        </soapenv:Body>
    </soapenv:Envelope>",[Session, User, User, ServiceProfile]).
    
%%%===================================================================
%%% Internal functions
%%%===================================================================
