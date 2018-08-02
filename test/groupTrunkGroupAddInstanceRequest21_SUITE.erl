%%%-------------------------------------------------------------------
 %%% File    : example_SUITE.erl
 %%% Author  : 
 %%% Description : 
 %%%
 %%% Created : 
 %%%-------------------------------------------------------------------
 -module(groupTrunkGroupAddInstanceRequest21_SUITE).

 -compile(export_all).
 -include_lib("common_test/include/ct.hrl").

 %%--------------------------------------------------------------------
 %% Function: suite() -> Info
 %% Info = [tuple()]
 %%--------------------------------------------------------------------
 suite() ->
     [{timetrap,{seconds,30}}].

 %%--------------------------------------------------------------------
 %% Function: init_per_suite(Config0) ->
 %%               Config1 | {skip,Reason} | {skip_and_save,Reason,Config1}
 %% Config0 = Config1 = [tuple()]
 %% Reason = term()
 %%--------------------------------------------------------------------
 init_per_suite(Config) ->
     
     SP = 'SP1',
     Grp = 'VK11111114',
     TrunkId = 'tggrp1',
     
     application:ensure_all_started(em),     
     [{sp, SP},{grp, Grp},{trunkid, TrunkId } | Config].
 %%--------------------------------------------------------------------
 %% Function: end_per_suite(Config0) -> term() | {save_config,Config1}
 %% Config0 = Config1 = [tuple()]
 %%--------------------------------------------------------------------
 end_per_suite(_Config) ->
     ok.

 %%--------------------------------------------------------------------
 %% Function: init_per_group(GroupName, Config0) ->
 %%               Config1 | {skip,Reason} | {skip_and_save,Reason,Config1}
 %% GroupName = atom()
 %% Config0 = Config1 = [tuple()]
 %% Reason = term()
 %%--------------------------------------------------------------------
 init_per_group(_GroupName, Config) ->
     Config.

 %%--------------------------------------------------------------------
 %% Function: end_per_group(GroupName, Config0) ->
 %%               term() | {save_config,Config1}
 %% GroupName = atom()
 %% Config0 = Config1 = [tuple()]
 %%--------------------------------------------------------------------
 end_per_group(_GroupName, _Config) ->
     ok.

 %%--------------------------------------------------------------------
 %% Function: init_per_testcase(TestCase, Config0) ->
 %%               Config1 | {skip,Reason} | {skip_and_save,Reason,Config1}
 %% TestCase = atom()
 %% Config0 = Config1 = [tuple()]
 %% Reason = term()
 %%--------------------------------------------------------------------
 init_per_testcase(_TestCase, Config) ->
     Config.

 %%--------------------------------------------------------------------
 %% Function: end_per_testcase(TestCase, Config0) ->
 %%               term() | {save_config,Config1} | {fail,Reason}
 %% TestCase = atom()
 %% Config0 = Config1 = [tuple()]
 %% Reason = term()
 %%--------------------------------------------------------------------
 end_per_testcase(_TestCase, Config) ->
    SP = ?config(sp, Config),
    Grp = ?config(grp, Config),
    TrunkId = ?config(user, Config),
      
    ok = em_event_server:process_event(nil_pilot(SP, Grp, TrunkId)),
    ok = em_event_server:process_event(delete_pilot_user('pilotuser@lab24.timezone4.com')),
    ok = em_event_server:process_event(delete(SP, Grp, TrunkId)).

 %%--------------------------------------------------------------------
 %% Function: groups() -> [Group]
 %% Group = {GroupName,Properties,GroupsAndTestCases}
 %% GroupName = atom()
 %% Properties = [parallel | sequence | Shuffle | {RepeatType,N}]
 %% GroupsAndTestCases = [Group | {group,GroupName} | TestCase]
 %% TestCase = atom()
 %% Shuffle = shuffle | {shuffle,{integer(),integer(),integer()}}
 %% RepeatType = repeat | repeat_until_all_ok | repeat_until_all_fail |
 %%              repeat_until_any_ok | repeat_until_any_fail
 %% N = integer() | forever
 %%--------------------------------------------------------------------
 groups() ->
     [].

 %%--------------------------------------------------------------------
 %% Function: all() -> GroupsAndTestCases | {skip,Reason}
 %% GroupsAndTestCases = [{group,GroupName} | TestCase]
 %% GroupName = atom()
 %% TestCase = atom()
 %% Reason = term()
 %%--------------------------------------------------------------------
 all() -> 
     [create_trunk].   

 %%--------------------------------------------------------------------
 %% Function: TestCase() -> Info
 %% Info = [tuple()]
 %%--------------------------------------------------------------------
create_trunk() ->
    []. 
    
 %%--------------------------------------------------------------------
 %% Function: TestCase(Config0) ->
 %%               ok | exit() | {skip,Reason} | {comment,Comment} |
 %%               {save_config,Config1} | {skip_and_save,Reason,Config1}
 %% Config0 = Config1 = [tuple()]
 %% Reason = term()
 %% Comment = term()
 %%--------------------------------------------------------------------

create_trunk(Config) ->
    SP = ?config(sp, Config),
    Grp = ?config(grp, Config),
    TrunkId = ?config(trunkid, Config),
      
    ok = em_event_server:process_event(add(SP, Grp, TrunkId)).
    
        
    
add(SP, Grp, TrunkId) ->
    io_lib:format("<BroadsoftOCIReportingDocument protocol=\"OCIReporting\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\">
	<command xsi:type=\"OCIReportingReportNotification\">
		<id>write274</id>
		<userId>admin</userId>
		<loginType>System</loginType>
		<request><![CDATA[<?xml version=\"1.0\" encoding=\"ISO-8859-1\"?>
			<BroadsoftDocument protocol=\"OCI\" xmlns=\"C\">
				<userId xmlns=\"\">admin</userId>
				<command xsi:type=\"GroupTrunkGroupAddInstanceRequest21\" xmlns=\"\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\">
					<serviceProviderId>~s</serviceProviderId>
					<groupId>~s</groupId>
					<name>~s</name>
					<pilotUser>
						<userId>pilotuser@lab24.timezone4.com</userId>
						<lastName>pilot</lastName>
						<firstName>pilot</firstName>
						<callingLineIdLastName>pilot</callingLineIdLastName>
						<callingLineIdFirstName>pilot</callingLineIdFirstName>
						<password>123456</password>
						<language>English</language>
						<timeZone>Europe/Copenhagen</timeZone>
						<linePort>pilotuser@lab24.timezone4.com</linePort>
					</pilotUser>
					<accessDevice>
						<deviceLevel>Group</deviceLevel>
						<deviceName>user005</deviceName>
					</accessDevice>
                    <maxActiveCalls>1</maxActiveCalls>
					<enableBursting>false</enableBursting>
					<capacityExceededTrapInitialCalls>0</capacityExceededTrapInitialCalls>
					<capacityExceededTrapOffsetCalls>0</capacityExceededTrapOffsetCalls>
					<invitationTimeout>6</invitationTimeout>
					<requireAuthentication>false</requireAuthentication>
					<sipAuthenticationUserName>123456</sipAuthenticationUserName>
					<sipAuthenticationPassword>123456</sipAuthenticationPassword>
					<trunkGroupIdentity>tggrp1@lab24.timezone4.com</trunkGroupIdentity>
					<allowTerminationToTrunkGroupIdentity>false</allowTerminationToTrunkGroupIdentity>
					<allowTerminationToDtgIdentity>false</allowTerminationToDtgIdentity>
					<includeTrunkGroupIdentity>false</includeTrunkGroupIdentity>
					<includeDtgIdentity>false</includeDtgIdentity>
					<includeTrunkGroupIdentityForNetworkCalls>false</includeTrunkGroupIdentityForNetworkCalls>
					<includeOtgIdentityForNetworkCalls>false</includeOtgIdentityForNetworkCalls>
					<enableNetworkAddressIdentity>false</enableNetworkAddressIdentity>
					<allowUnscreenedCalls>false</allowUnscreenedCalls>
					<allowUnscreenedEmergencyCalls>false</allowUnscreenedEmergencyCalls>
					<pilotUserCallingLineIdentityForExternalCallsPolicy>No Calls</pilotUserCallingLineIdentityForExternalCallsPolicy>
					<pilotUserChargeNumberPolicy>No Calls</pilotUserChargeNumberPolicy>
					<routeToPeeringDomain>false</routeToPeeringDomain>
					<prefixEnabled>false</prefixEnabled>
					<statefulReroutingEnabled>false</statefulReroutingEnabled>
					<sendContinuousOptionsMessage>false</sendContinuousOptionsMessage>
					<continuousOptionsSendingIntervalSeconds>30</continuousOptionsSendingIntervalSeconds>
					<failureOptionsSendingIntervalSeconds>10</failureOptionsSendingIntervalSeconds>
					<failureThresholdCounter>1</failureThresholdCounter>
					<successThresholdCounter>1</successThresholdCounter>
					<inviteFailureThresholdCounter>1</inviteFailureThresholdCounter>
					<inviteFailureThresholdWindowSeconds>30</inviteFailureThresholdWindowSeconds>
					<pilotUserCallingLineAssertedIdentityPolicy>Unscreened Originating Calls</pilotUserCallingLineAssertedIdentityPolicy>
                    <useSystemCallingLineAssertedIdentityPolicy>true</useSystemCallingLineAssertedIdentityPolicy>
					<pilotUserCallOptimizationPolicy>Optimize For User Services</pilotUserCallOptimizationPolicy>
					<clidSourceForScreenedCallsPolicy>Profile Name Profile Number</clidSourceForScreenedCallsPolicy>
					<useSystemCLIDSourceForScreenedCallsPolicy>true</useSystemCLIDSourceForScreenedCallsPolicy>
					<userLookupPolicy>Basic</userLookupPolicy>
					<useSystemUserLookupPolicy>true</useSystemUserLookupPolicy>
					<pilotUserCallingLineIdentityForEmergencyCallsPolicy>No Calls</pilotUserCallingLineIdentityForEmergencyCallsPolicy>
					<implicitRegistrationSetSupportPolicy>Disabled</implicitRegistrationSetSupportPolicy>
					<useSystemImplicitRegistrationSetSupportPolicy>true</useSystemImplicitRegistrationSetSupportPolicy>
					<sipIdentityForPilotAndProxyTrunkModesPolicy>User</sipIdentityForPilotAndProxyTrunkModesPolicy>
                    <useSystemSIPIdentityForPilotAndProxyTrunkModesPolicy>true</useSystemSIPIdentityForPilotAndProxyTrunkModesPolicy>
					<useSystemSupportConnectedIdentityPolicy>true</useSystemSupportConnectedIdentityPolicy>
					<supportConnectedIdentityPolicy>Disabled</supportConnectedIdentityPolicy>                    
				</command>
			</BroadsoftDocument>]]>
		</request>
	</command>
</BroadsoftOCIReportingDocument>",[SP, Grp, TrunkId]).


nil_pilot(SP, Grp, TrunkId) ->
    io_lib:format("<BroadsoftOCIReportingDocument protocol=\"OCIReporting\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\">
    <command xsi:type=\"OCIReportingReportNotification\">
        <id>write278</id>
        <userId>admin</userId>
        <loginType>System</loginType>
        <request><![CDATA[<?xml version=\"1.0\" encoding=\"ISO-8859-1\"?>
            <BroadsoftDocument protocol=\"OCI\" xmlns=\"C\"><userId xmlns=\"\">admin</userId>
                <command xsi:type=\"GroupTrunkGroupModifyInstanceRequest20sp1\" xmlns=\"\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\">
                    <trunkGroupKey>
                        <serviceProviderId>~s</serviceProviderId>
                        <groupId>~s</groupId>
                        <name>~s</name>
                    </trunkGroupKey>
                    <newName>tggrp1</newName>
                    <pilotUserId xsi:nil=\"true\"/>
                    <department xsi:type=\"GroupDepartmentKey\" xsi:nil=\"true\"/>
                    <maxActiveCalls>1</maxActiveCalls>
                    <maxIncomingCalls xsi:nil=\"true\"/>
                    <maxOutgoingCalls xsi:nil=\"true\"/>
                    <requireAuthentication>false</requireAuthentication>
                    <sipAuthenticationUserName>123456</sipAuthenticationUserName>
                    <trunkGroupIdentity>tggrp1@lab24.timezone4.com</trunkGroupIdentity>
                    <otgDtgIdentity xsi:nil=\"true\"/>
                    <allowTerminationToTrunkGroupIdentity>false</allowTerminationToTrunkGroupIdentity>
                    <allowTerminationToDtgIdentity>false</allowTerminationToDtgIdentity>
                    <includeTrunkGroupIdentity>false</includeTrunkGroupIdentity>
                    <includeDtgIdentity>false</includeDtgIdentity>
                    <includeTrunkGroupIdentityForNetworkCalls>false</includeTrunkGroupIdentityForNetworkCalls>
                    <includeOtgIdentityForNetworkCalls>false</includeOtgIdentityForNetworkCalls>
                    <enableNetworkAddressIdentity>false</enableNetworkAddressIdentity>
                    <allowUnscreenedCalls>false</allowUnscreenedCalls>
                    <allowUnscreenedEmergencyCalls>false</allowUnscreenedEmergencyCalls>
                    <pilotUserCallingLineIdentityForExternalCallsPolicy>No Calls</pilotUserCallingLineIdentityForExternalCallsPolicy>
                    <pilotUserChargeNumberPolicy>No Calls</pilotUserChargeNumberPolicy>
                    <peeringDomain xsi:nil=\"true\"/>
                    <routeToPeeringDomain>false</routeToPeeringDomain>
                    <prefixEnabled>false</prefixEnabled>
                    <prefix xsi:nil=\"true\"/>
                    <pilotUserCallingLineAssertedIdentityPolicy>Unscreened Originating Calls</pilotUserCallingLineAssertedIdentityPolicy>
                    <useSystemCallingLineAssertedIdentityPolicy>true</useSystemCallingLineAssertedIdentityPolicy>
                    <pilotUserCallOptimizationPolicy>Optimize For User Services</pilotUserCallOptimizationPolicy>
                    <clidSourceForScreenedCallsPolicy>Profile Name Profile Number</clidSourceForScreenedCallsPolicy>
                    <useSystemCLIDSourceForScreenedCallsPolicy>true</useSystemCLIDSourceForScreenedCallsPolicy>
                    <userLookupPolicy>Basic</userLookupPolicy>
                    <useSystemUserLookupPolicy>true</useSystemUserLookupPolicy>
                    <pilotUserCallingLineIdentityForEmergencyCallsPolicy>No Calls</pilotUserCallingLineIdentityForEmergencyCallsPolicy>
                    <implicitRegistrationSetSupportPolicy>Disabled</implicitRegistrationSetSupportPolicy>
                    <useSystemImplicitRegistrationSetSupportPolicy>true</useSystemImplicitRegistrationSetSupportPolicy>
                    <sipIdentityForPilotAndProxyTrunkModesPolicy>User</sipIdentityForPilotAndProxyTrunkModesPolicy>
                    <useSystemSIPIdentityForPilotAndProxyTrunkModesPolicy>true</useSystemSIPIdentityForPilotAndProxyTrunkModesPolicy>
                    <useSystemSupportConnectedIdentityPolicy>true</useSystemSupportConnectedIdentityPolicy>
                    <supportConnectedIdentityPolicy>Disabled</supportConnectedIdentityPolicy>
                </command>
            </BroadsoftDocument>]]>
        </request>
    </command>
</BroadsoftOCIReportingDocument>",[SP, Grp, TrunkId]).

delete_pilot_user(User) ->
    io_lib:format("<BroadsoftOCIReportingDocument protocol=\"OCIReporting\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\">
    <command xsi:type=\"OCIReportingReportNotification\">
        <id>write279</id>
        <userId>admin</userId>
        <loginType>System</loginType>
        <request><![CDATA[<?xml version=\"1.0\" encoding=\"ISO-8859-1\"?>
            <BroadsoftDocument protocol=\"OCI\" xmlns=\"C\">
                <userId xmlns=\"\">admin</userId>
                <command xsi:type=\"UserDeleteRequest\" xmlns=\"\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\">
                    <userId>~s</userId>
                </command>
            </BroadsoftDocument>]]>
        </request>
    </command>
</BroadsoftOCIReportingDocument>",[User]).
                    
delete(SP, Grp, TrunkId) ->
    io_lib:format("<BroadsoftOCIReportingDocument protocol=\"OCIReporting\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\">
    <command xsi:type=\"OCIReportingReportNotification\">
        <id>write280</id>
        <userId>admin</userId>
        <loginType>System</loginType>
        <request><![CDATA[<?xml version=\"1.0\" encoding=\"ISO-8859-1\"?>
            <BroadsoftDocument protocol=\"OCI\" xmlns=\"C\">
                <userId xmlns=\"\">admin</userId>
                <command xsi:type=\"GroupTrunkGroupDeleteInstanceRequest14sp4\" xmlns=\"\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\">
                    <trunkGroupKey>
                        <serviceProviderId>~s</serviceProviderId>
                        <groupId>~s</groupId>
                        <name>~s</name>
                    </trunkGroupKey>
                </command>
            </BroadsoftDocument>]]>
        </request>
    </command>
</BroadsoftOCIReportingDocument>",[SP, Grp, TrunkId]).

    
    
