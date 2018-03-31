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
    add_ims_subscriber/2,
    add_ims_virtual_subscriber/2,
    add_ims_teluri/2,
    add_ims_pubid/2,
    delete_ims_teluri/3,
    delete_ims_subscriber/2,
    delete_ims_pubid/3,
    add_ims_serviceprofile/2,
    delete_ims_serviceprofile/3,
    add_ims_enum/2,
    delete_ims_enum/2,
    set_ims_pass/3,
    set_ims_phonecontext/3
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

add_ims_subscriber(Session, IMSAssociation) ->
    User = maps:get(user, IMSAssociation),
    Pass = maps:get(pass, IMSAssociation),
    AssociationId = maps:get(association, IMSAssociation),
    io_lib:format(
    "<soapenv:Envelope xmlns:soapenv=\"http://schemas.xmlsoap.org/soap/envelope/\" xmlns:cai3=\"http://schemas.ericsson.com/cai3g1.2/\" xmlns:hss=\"http://schemas.ericsson.com/ma/HSS/\">
   <soapenv:Header>
      <cai3:SessionId>~s</cai3:SessionId>
   </soapenv:Header>
   <soapenv:Body>
      <cai3:Create>
         <cai3:MOType>IMSAssociation@http://schemas.ericsson.com/ma/HSS/</cai3:MOType>
         <cai3:MOId>
            <hss:associationId>~s</hss:associationId>
         </cai3:MOId>
         <cai3:MOAttributes>
            <hss:CreateIMSAssociation associationId=\"~s\">
               <hss:associationId>~s</hss:associationId>
               <hss:chargingProfId>DefaultChargingProfile</hss:chargingProfId>
               <hss:isPsi>false</hss:isPsi>
               <hss:privateUser privateUserId=\"~s\">
                  <hss:privateUserId>~s</hss:privateUserId>
                  <hss:userPassword>~s</hss:userPassword>
                  <hss:allowedAuthMechanism>DIGEST</hss:allowedAuthMechanism>
               </hss:privateUser>               
            </hss:CreateIMSAssociation>
         </cai3:MOAttributes>
      </cai3:Create>
   </soapenv:Body>
</soapenv:Envelope>",[Session, AssociationId, AssociationId, AssociationId, User, User, Pass]).
    
add_ims_virtual_subscriber(Session, IMSAssociation) ->
    User = maps:get(user, IMSAssociation),
    AssociationId = maps:get(association, IMSAssociation),    
    io_lib:format(
    "<soapenv:Envelope xmlns:soapenv=\"http://schemas.xmlsoap.org/soap/envelope/\" xmlns:cai3=\"http://schemas.ericsson.com/cai3g1.2/\" xmlns:hss=\"http://schemas.ericsson.com/ma/HSS/\">
   <soapenv:Header>
      <cai3:SessionId>~s</cai3:SessionId>
   </soapenv:Header>
   <soapenv:Body>
      <cai3:Create>
         <cai3:MOType>IMSAssociation@http://schemas.ericsson.com/ma/HSS/</cai3:MOType>
         <cai3:MOId>
            <hss:associationId>~s</hss:associationId>
         </cai3:MOId>
         <cai3:MOAttributes>
            <hss:CreateIMSAssociation associationId=\"~s\">
               <hss:associationId>~s</hss:associationId>
               <hss:chargingProfId>DefaultChargingProfile</hss:chargingProfId>
               <hss:isPsi>true</hss:isPsi>
               <hss:privateUser privateUserId=\"~s\">
                  <hss:privateUserId>~s</hss:privateUserId>
               </hss:privateUser>               
            </hss:CreateIMSAssociation>
         </cai3:MOAttributes>
      </cai3:Create>
   </soapenv:Body>
</soapenv:Envelope>
",[Session, AssociationId, AssociationId, AssociationId, User, User]).    

add_ims_teluri(Session, IMSAssociation) ->
    User = maps:get(user, IMSAssociation),
    PubId = maps:get(pubid, IMSAssociation),
    Phone = maps:get(phone, IMSAssociation),
    IRS = maps:get(irs, IMSAssociation),
    IsDefault = maps:get(isdefault, IMSAssociation),
    AssociationId = maps:get(association, IMSAssociation), 
    
    io_lib:format(
    "<soapenv:Envelope xmlns:soapenv=\"http://schemas.xmlsoap.org/soap/envelope/\" xmlns:cai3=\"http://schemas.ericsson.com/cai3g1.2/\" xmlns:hss=\"http://schemas.ericsson.com/ma/HSS/\">
   <soapenv:Header>
      <cai3:SessionId>~s</cai3:SessionId>
   </soapenv:Header>
   <soapenv:Body>
      <cai3:Set>
         <cai3:MOType>IMSAssociation@http://schemas.ericsson.com/ma/HSS/</cai3:MOType>
         <cai3:MOId>
            <hss:associationId>~s</hss:associationId>
         </cai3:MOId>
         <cai3:MOAttributes>
            <hss:SetIMSAssociation associationId=\"~s\">
               <hss:publicData publicIdValue=\"tel:+299~s\">
                  <hss:publicIdValue>tel:+299~s</hss:publicIdValue>
                  <hss:privateUserId>~s</hss:privateUserId>
                  <hss:implicitRegSet>~s</hss:implicitRegSet>
                  <hss:isDefault>~s</hss:isDefault>
                  <hss:serviceProfileId>~s</hss:serviceProfileId>
                  <hss:sessionBarringInd>false</hss:sessionBarringInd>
               </hss:publicData>
            </hss:SetIMSAssociation>
         </cai3:MOAttributes>
      </cai3:Set>
   </soapenv:Body>
  </soapenv:Envelope>",[Session, AssociationId, AssociationId, Phone, Phone, User, IRS, IsDefault, PubId]).   

    
delete_ims_teluri(Session, User, Phone) ->
    io_lib:format(
    "<soapenv:Envelope xmlns:soapenv=\"http://schemas.xmlsoap.org/soap/envelope/\" xmlns:cai3=\"http://schemas.ericsson.com/cai3g1.2/\" xmlns:hss=\"http://schemas.ericsson.com/ma/HSS/\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\">
   <soapenv:Header>
      <cai3:SessionId>~s</cai3:SessionId>
   </soapenv:Header>
   <soapenv:Body>
      <cai3:Set>
         <cai3:MOType>IMSAssociation@http://schemas.ericsson.com/ma/HSS/</cai3:MOType>
         <cai3:MOId>
            <hss:associationId>~s</hss:associationId>
         </cai3:MOId>
         <cai3:MOAttributes>
            <hss:SetIMSAssociation associationId=\"~s\">
               <hss:publicData publicIdValue=\"tel:+299~s\" xsi:nil=\"true\"/>
            </hss:SetIMSAssociation>
         </cai3:MOAttributes>
      </cai3:Set>
   </soapenv:Body>
</soapenv:Envelope>",[Session, User, User, Phone]).

delete_ims_subscriber(Session, IMSAssociation) ->
    AssociationId = maps:get(association, IMSAssociation),  
    io_lib:format(
    "<soapenv:Envelope xmlns:soapenv=\"http://schemas.xmlsoap.org/soap/envelope/\" xmlns:cai3=\"http://schemas.ericsson.com/cai3g1.2/\" xmlns:hss=\"http://schemas.ericsson.com/ma/HSS/\">
   <soapenv:Header>
      <cai3:SessionId>~s</cai3:SessionId>
   </soapenv:Header>
   <soapenv:Body>
      <cai3:Delete>
         <cai3:MOType>IMSAssociation@http://schemas.ericsson.com/ma/HSS/</cai3:MOType>
         <cai3:MOId>
            <hss:associationId>~s</hss:associationId>
         </cai3:MOId>
      </cai3:Delete>
   </soapenv:Body>
</soapenv:Envelope>",[Session, AssociationId]).
    
add_ims_pubid(Session, IMSAssociation) ->
    User = maps:get(user, IMSAssociation),
    PubId = maps:get(pubid, IMSAssociation),
    IRS = maps:get(irs, IMSAssociation),
    IsDefault = maps:get(isdefault, IMSAssociation),
    AssociationId = maps:get(association, IMSAssociation),
    SProfile = PubId,
    io_lib:format(
    "<soapenv:Envelope xmlns:soapenv=\"http://schemas.xmlsoap.org/soap/envelope/\" xmlns:cai3=\"http://schemas.ericsson.com/cai3g1.2/\" xmlns:hss=\"http://schemas.ericsson.com/ma/HSS/\">
   <soapenv:Header>
      <cai3:SessionId>~s</cai3:SessionId>
   </soapenv:Header>
   <soapenv:Body>
      <cai3:Set>
         <cai3:MOType>IMSAssociation@http://schemas.ericsson.com/ma/HSS/</cai3:MOType>
         <cai3:MOId>
            <hss:associationId>~s</hss:associationId>
         </cai3:MOId>
         <cai3:MOAttributes>
            <hss:SetIMSAssociation associationId=\"~s\">
               <hss:publicData publicIdValue=\"sip:~s\">
                  <hss:publicIdValue>sip:~s</hss:publicIdValue>
                  <hss:privateUserId>~s</hss:privateUserId>
                  <hss:implicitRegSet>~s</hss:implicitRegSet>
                  <hss:isDefault>~s</hss:isDefault>
                  <hss:serviceProfileId>~s</hss:serviceProfileId>
                  <hss:sessionBarringInd>false</hss:sessionBarringInd>
               </hss:publicData>
            </hss:SetIMSAssociation>
         </cai3:MOAttributes>
      </cai3:Set>
   </soapenv:Body>
</soapenv:Envelope>",[Session, AssociationId, AssociationId, PubId, PubId, User, IRS, IsDefault, SProfile]).    

     
delete_ims_pubid(Session, AssociationId, CurrentPubId) ->
     io_lib:format(
     "<soapenv:Envelope xmlns:soapenv=\"http://schemas.xmlsoap.org/soap/envelope/\" xmlns:cai3=\"http://schemas.ericsson.com/cai3g1.2/\" xmlns:hss=\"http://schemas.ericsson.com/ma/HSS/\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\">
    <soapenv:Header>
       <cai3:SessionId>~s</cai3:SessionId>
    </soapenv:Header>
    <soapenv:Body>
       <cai3:Set>
          <cai3:MOType>IMSAssociation@http://schemas.ericsson.com/ma/HSS/</cai3:MOType>
          <cai3:MOId>
             <hss:associationId>~s</hss:associationId>
          </cai3:MOId>
          <cai3:MOAttributes>
             <hss:SetIMSAssociation associationId=\"~s\">
                <hss:publicData publicIdValue=\"sip:~s\" xsi:nil=\"true\"/>
             </hss:SetIMSAssociation>
          </cai3:MOAttributes>
       </cai3:Set>
    </soapenv:Body>
 </soapenv:Envelope>",[Session, AssociationId, AssociationId, CurrentPubId]).

add_ims_serviceprofile(Session, IMSAssociation) ->
    PubId = maps:get(pubid, IMSAssociation),
    SProfile = PubId,
    CSProfile = maps:get(csprofile, IMSAssociation),
    AssociationId = maps:get(association, IMSAssociation),
    
    io_lib:format(
    "<soapenv:Envelope xmlns:soapenv=\"http://schemas.xmlsoap.org/soap/envelope/\" xmlns:cai3=\"http://schemas.ericsson.com/cai3g1.2/\" xmlns:hss=\"http://schemas.ericsson.com/ma/HSS/\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\">
   <soapenv:Header>
      <cai3:SessionId>~s</cai3:SessionId>
   </soapenv:Header>
   <soapenv:Body>
      <cai3:Set>
         <cai3:MOType>IMSAssociation@http://schemas.ericsson.com/ma/HSS/</cai3:MOType>
         <cai3:MOId>
            <hss:associationId>~s</hss:associationId>
         </cai3:MOId>
         <cai3:MOAttributes>
            <hss:SetIMSAssociation associationId=\"~s\">
               <hss:subscriberServiceProfile serviceProfileId=\"~s\">
                  <hss:serviceProfileId>~s</hss:serviceProfileId>
                  <hss:configuredServiceProfile configuredServiceProfileId=\"~s\">
                     <hss:configuredServiceProfileId>~s</hss:configuredServiceProfileId>
                  </hss:configuredServiceProfile>
               </hss:subscriberServiceProfile>
            </hss:SetIMSAssociation>
         </cai3:MOAttributes>
      </cai3:Set>
   </soapenv:Body>
</soapenv:Envelope>",[Session, AssociationId, AssociationId, SProfile, SProfile, CSProfile, CSProfile]).

delete_ims_serviceprofile(Session, AssociationId, SProfile) ->
    io_lib:format(
    "<soapenv:Envelope xmlns:soapenv=\"http://schemas.xmlsoap.org/soap/envelope/\" xmlns:cai3=\"http://schemas.ericsson.com/cai3g1.2/\" xmlns:hss=\"http://schemas.ericsson.com/ma/HSS/\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\">
   <soapenv:Header>
      <cai3:SessionId>~s</cai3:SessionId>
   </soapenv:Header>
   <soapenv:Body>
      <cai3:Set>
         <cai3:MOType>IMSAssociation@http://schemas.ericsson.com/ma/HSS/</cai3:MOType>
         <cai3:MOId>
            <hss:associationId>~s</hss:associationId>
         </cai3:MOId>
         <cai3:MOAttributes>
            <hss:SetIMSAssociation associationId=\"~s\">
               <hss:subscriberServiceProfile serviceProfileId=\"~s\" xsi:nil=\"true\">
               </hss:subscriberServiceProfile>
            </hss:SetIMSAssociation>
         </cai3:MOAttributes>
      </cai3:Set>
   </soapenv:Body>
</soapenv:Envelope>",[Session, AssociationId, AssociationId, SProfile]).

add_ims_enum(Session, IMSAssociation) ->
    PubId = maps:get(pubid, IMSAssociation),
    Phone = maps:get(phone, IMSAssociation),
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

delete_ims_enum(Session, Phone) ->
    io_lib:format(
    "<soapenv:Envelope xmlns:soapenv=\"http://schemas.xmlsoap.org/soap/envelope/\" xmlns:cai3=\"http://schemas.ericsson.com/cai3g1.2/\" xmlns:ns=\"http://schemas.ericsson.com/ema/UserProvisioning/IPWorks/5.0/\">
   <soapenv:Header>
      <cai3:SessionId>~s</cai3:SessionId>
   </soapenv:Header>
   <soapenv:Body>
      <cai3:Delete>
         <cai3:MOType>DNSSubscription@http://schemas.ericsson.com/ema/UserProvisioning/IPWorks/5.0/</cai3:MOType>
         <cai3:MOId>
            <ns:msisdn>tel:299~s</ns:msisdn>
         </cai3:MOId>
         <cai3:MOAttributes>
            <ns:deleteDNSSubscription msisdn=\"tel:299~s\">
            </ns:deleteDNSSubscription>
         </cai3:MOAttributes>
      </cai3:Delete>
   </soapenv:Body>
</soapenv:Envelope>",[Session, Phone, Phone]).
    
set_ims_pass(Session, User, Pass) ->
    io_lib:format(
    "<soapenv:Envelope xmlns:soapenv=\"http://schemas.xmlsoap.org/soap/envelope/\" xmlns:cai3=\"http://schemas.ericsson.com/cai3g1.2/\" xmlns:hss=\"http://schemas.ericsson.com/ma/HSS/\">
   <soapenv:Header>
      <cai3:SessionId>~s</cai3:SessionId>
   </soapenv:Header>
   <soapenv:Body>
      <cai3:Set>
         <cai3:MOType>IMSAssociation@http://schemas.ericsson.com/ma/HSS/</cai3:MOType>
         <cai3:MOId>
            <hss:associationId>~s</hss:associationId>
         </cai3:MOId>
         <cai3:MOAttributes>
            <hss:SetIMSAssociation associationId=\"~s\">
               <hss:privateUser privateUserId=\"~s\">
                  <hss:userPassword>~s</hss:userPassword>
               </hss:privateUser>
            </hss:SetIMSAssociation>
         </cai3:MOAttributes>
      </cai3:Set>
   </soapenv:Body>
</soapenv:Envelope>",[Session, User, User, User, Pass]).
    
set_ims_phonecontext(Session, User, PhoneContext) ->
    io_lib:format(
    "<soapenv:Envelope xmlns:soapenv=\"http://schemas.xmlsoap.org/soap/envelope/\" xmlns:cai3=\"http://schemas.ericsson.com/cai3g1.2/\" xmlns:hss=\"http://schemas.ericsson.com/ma/HSS/\">
   <soapenv:Header>
      <cai3:SessionId>~s</cai3:SessionId>
   </soapenv:Header>
   <soapenv:Body>
      <cai3:Set>
         <cai3:MOType>IMSAssociation@http://schemas.ericsson.com/ma/HSS/</cai3:MOType>
         <cai3:MOId>
            <hss:associationId>~s</hss:associationId>
         </cai3:MOId>
         <cai3:MOAttributes>
            <hss:SetIMSAssociation associationId=\"~s\">
               <hss:subscriberServiceProfile serviceProfileId=\"~s\">
                  <hss:phoneContext>~s</hss:phoneContext>
               </hss:subscriberServiceProfile>
            </hss:SetIMSAssociation>
         </cai3:MOAttributes>
      </cai3:Set>
   </soapenv:Body>
</soapenv:Envelope>",[Session, User, User, User, PhoneContext]).
%%%===================================================================
%%% Internal functions
%%%===================================================================
