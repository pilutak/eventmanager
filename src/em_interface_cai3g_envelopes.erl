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
-include("../include/em.hrl").

-export([
    login/2,
    logout/1,
    add_subscriber/4,
    add_teluri/4,
    add_pubid/4,
    delete_teluri/3,
    delete_subscriber/2,
    delete_pubid/3,
    add_serviceprofile/4,
    delete_serviceprofile/3,
    add_enum/3,
    delete_enum/3
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

add_subscriber(Session, User, CSProfile, SProfile) ->
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
    </soapenv:Envelope>",[Session, User, User, User, User, User, User, User, User, User, SProfile, SProfile, CSProfile, CSProfile]).

add_teluri(Session, User, Phone, PubId) ->
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
                        <publicData publicIdValue=\"tel:+299~s\">
                            <publicIdValue>tel:+299~s</publicIdValue>
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
    </soapenv:Envelope>",[Session, User, User, Phone, Phone, User, PubId]).

delete_teluri(Session, User, Phone) ->
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
                        <publicData publicIdValue=\"tel:+299~s\">
                            <publicIdState>not_registered</publicIdState>
                        </publicData>
                        <publicData publicIdValue=\"tel:+299~s\" xsi:nil=\"true\"/>
                    </SetISMSubscription>
                </cai3:MOAttributes>
            </cai3:Set>
        </soapenv:Body>
    </soapenv:Envelope>",[Session, User, User, Phone, Phone]).

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
    
add_pubid(Session, User, PubId, SProfile) ->
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
                        <publicData publicIdValue=\"sip:~s\">
                            <publicIdValue>sip:~s</publicIdValue>
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
    </soapenv:Envelope>",[Session, User, User, PubId, PubId, User, SProfile]).
    
delete_pubid(Session, User, PubId) ->
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
                        <publicData publicIdValue=\"sip:~s\">
                            <publicIdState>not_registered</publicIdState>
                        </publicData>
                        <publicData publicIdValue=\"sip:~s\" xsi:nil=\"true\"/>
                    </SetISMSubscription>
                </cai3:MOAttributes>
            </cai3:Set>
        </soapenv:Body>
    </soapenv:Envelope>",[Session, User, User, PubId, PubId]).
    

add_serviceprofile(Session, User, SProfile, CSProfile) ->
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
    </soapenv:Envelope>",[Session, User, User, SProfile, SProfile, CSProfile, CSProfile]).
        
delete_serviceprofile(Session, User, SProfile) ->
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
    </soapenv:Envelope>",[Session, User, User, SProfile]).
    
add_enum(Session, Phone, PubId) ->
    io_lib:format(
    "<soapenv:Envelope xmlns:soapenv=\"http://schemas.xmlsoap.org/soap/envelope/\" xmlns:cai3=\"http://schemas.ericsson.com/cai3g1.2/\" xmlns:ns=\"http://schemas.ericsson.com/ema/UserProvisioning/IPWorks/5.0/\"> 
   <soapenv:Header> 
      <cai3:SessionId>~s</cai3:SessionId> 
   </soapenv:Header> 
   <soapenv:Body> 
      <cai3:Create> 
         <cai3:MOType>DNSSubscription@http://schemas.ericsson.com/ema/UserProvisioning/IPWorks/5.0/</cai3:MOType> 
         <cai3:MOId> 
            <cai3:msisdn>tel:299~s</cai3:msisdn> 
         </cai3:MOId> 
         <cai3:MOAttributes> 
            <ns:createDNSSubscription msisdn=\"tel:299~s\">
               <ns:msisdn>tel:299~s</ns:msisdn>
               <ns:subscriberId>NONE</ns:subscriberId> 
               <ns:records publicId=\"sip:~s\"> 
                  <ns:publicId>sip:~s</ns:publicId> 
                  <ns:flags>nu</ns:flags> 
                  <ns:order>10</ns:order> 
                  <ns:preference>10</ns:preference> 
                  <ns:service>E2U+sip</ns:service>
               </ns:records> 
            </ns:createDNSSubscription> 
         </cai3:MOAttributes> 
      </cai3:Create> 
   </soapenv:Body> 
</soapenv:Envelope>",[Session, Phone, Phone, Phone, PubId, PubId]).

delete_enum(Session, Phone, PubId) ->
    io_lib:format(
    "<soapenv:Envelope xmlns:soapenv=\"http://schemas.xmlsoap.org/soap/envelope/\" xmlns:cai3=\"http://schemas.ericsson.com/cai3g1.2/\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" xmlns:ns=\"http://schemas.ericsson.com/ema/UserProvisioning/IPWorks/5.0/\">    
    <soapenv:Header> 
      <cai3:SessionId>~s</cai3:SessionId> 
   </soapenv:Header> 
   <soapenv:Body> 
      <cai3:Delete>
         <cai3:MOType>DNSSubscription@http://schemas.ericsson.com/ema/UserProvisioning/IPWorks/5.0/</cai3:MOType>
         <cai3:MOId>
            <cai3:msisdn>tel:299~s</cai3:msisdn>
         </cai3:MOId>
         <cai3:MOAttributes> 
            <ns:setDNSSubscription msisdn=\"tel:299~s\"> 
               <ns:subscriberId>NONE</ns:subscriberId> 
               <ns:records publicId=\"sip:~s\" xsi:nil=\"true\"/> 
            </ns:setDNSSubscription> 
         </cai3:MOAttributes> 
      </cai3:Delete>
   </soapenv:Body> 
</soapenv:Envelope> ",[Session, Phone, Phone, PubId]).

%%%===================================================================
%%% Internal functions
%%%===================================================================
