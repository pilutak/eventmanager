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

%% API
-export([env_login/2,env_logout/1,env_create_hss_subscriber/3,env_add_hss_tel_uri/3,env_add_hss_sip_uri/3,env_delete_hss_subscriber/2]).


env_login(UserID,Password)->
 io_lib:format("<soapenv:Envelope xmlns:soapenv=\"http://schemas.xmlsoap.org/soap/envelope/\" xmlns:cai3=\"http://schemas.ericsson.com/cai3g1.2/\">
  <soapenv:Header/>
  <soapenv:Body>
    <cai3:Login>
      <cai3:userId>~s</cai3:userId>
      <cai3:pwd>~s</cai3:pwd>
    </cai3:Login>
  </soapenv:Body>
  </soapenv:Envelope>",[UserID,Password]).

env_logout(SessionID)->
  io_lib:format("<soapenv:Envelope xmlns:soapenv=\"http://schemas.xmlsoap.org/soap/envelope/\" xmlns:cai3=\"http://schemas.ericsson.com/cai3g1.2/\">
  <soapenv:Header>
    <cai3:SessionId>~s</cai3:SessionId>
  </soapenv:Header>
  <soapenv:Body>
    <cai3:Logout>
      <cai3:sessionId>~s</cai3:sessionId>
    </cai3:Logout>
  </soapenv:Body>
</soapenv:Envelope>",[SessionID,SessionID]).

env_create_hss_subscriber(SessionID, UserID, ConfServiceProfile)->
  io_lib:format("<soapenv:Envelope xmlns:soapenv=\"http://schemas.xmlsoap.org/soap/envelope/\" xmlns:cai3=\"http://schemas.ericsson.com/cai3g1.2/\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\">
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
</soapenv:Envelope>",[SessionID,UserID,UserID,UserID,UserID,UserID,UserID,UserID,ConfServiceProfile,ConfServiceProfile]).


env_add_hss_tel_uri(SessionID,UserID,TelURI)->
  io_lib:format("<soapenv:Envelope xmlns:soapenv=\"http://schemas.xmlsoap.org/soap/envelope/\" xmlns:cai3=\"http://schemas.ericsson.com/cai3g1.2/\" xmlns:ns=\"http://schemas.ericsson.com/ema/UserProvisioning/HSS/5.0/\">
   <soapenv:Header>
      <cai3:SessionId>~s</cai3:SessionId>
   </soapenv:Header>
   <soapenv:Body>
      <cai3:Set>
         <cai3:MOType>HSSSubscription@http://schemas.ericsson.com/ema/UserProvisioning/HSS/5.0/</cai3:MOType>
         <cai3:MOId>
            <cai3:subscriberId>~s</cai3:subscriberId>
         </cai3:MOId>
         <cai3:MOAttributes>
            <ns:setHSSSubscription subscriberId=\"~s\">
            	<ns:pubData publicIdValue=\"sip:~s\">
                  	<ns:privateUserId>~s</ns:privateUserId>
                  	<ns:publicIdTelValue>tel:~s</ns:publicIdTelValue>
                  	<ns:implicitRegSet>0</ns:implicitRegSet>
                  	<ns:isDefault>FALSE</ns:isDefault>
                  <ns:maxSessions>99</ns:maxSessions>
               </ns:pubData>
		</ns:setHSSSubscription>
         </cai3:MOAttributes>
      </cai3:Set>
   </soapenv:Body>
</soapenv:Envelope>",[SessionID,UserID,UserID,UserID,UserID,TelURI]).

env_add_hss_sip_uri(SessionID,UserID,SipURI)->
  io_lib:format("<soapenv:Envelope xmlns:soapenv=\"http://schemas.xmlsoap.org/soap/envelope/\" xmlns:cai3=\"http://schemas.ericsson.com/cai3g1.2/\" xmlns:ns=\"http://schemas.ericsson.com/ema/UserProvisioning/HSS/5.0/\">
   <soapenv:Header>
      <cai3:SessionId>~s</cai3:SessionId>
   </soapenv:Header>
   <soapenv:Body>
      <cai3:Set>
         <cai3:MOType>HSSSubscription@http://schemas.ericsson.com/ema/UserProvisioning/HSS/5.0/</cai3:MOType>
         <cai3:MOId>
            <cai3:subscriberId>~s</cai3:subscriberId>
         </cai3:MOId>
         <cai3:MOAttributes>
            <ns:setHSSSubscription subscriberId=\"~s\">
            	<ns:pubData publicIdValue=\"sip:~s\">
               	<ns:publicIdValue>sip:~s</ns:publicIdValue>
                  	<ns:privateUserId>~s</ns:privateUserId>
                  	<ns:implicitRegSet>0</ns:implicitRegSet>
                  	<ns:isDefault>FALSE</ns:isDefault>
                  	<ns:configuredServiceProfiles configuredServiceProfileId=\"IMS_CENTREX\">
                    	<ns:configuredServiceProfileId>IMS_CENTREX</ns:configuredServiceProfileId>
                  	</ns:configuredServiceProfiles>
                  <ns:maxSessions>99</ns:maxSessions>
               </ns:pubData>
		</ns:setHSSSubscription>
         </cai3:MOAttributes>
      </cai3:Set>
   </soapenv:Body>
</soapenv:Envelope>",[SessionID,UserID,UserID,SipURI,SipURI,UserID]).

env_delete_hss_subscriber(SessionID, UserID)->
  io_lib:format("<soapenv:Envelope xmlns:soapenv=\"http://schemas.xmlsoap.org/soap/envelope/\" xmlns:cai3=\"http://schemas.ericsson.com/cai3g1.2/\">
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
</soapenv:Envelope>",[SessionID,UserID]).

