%%%-------------------------------------------------------------------
 %%% File    : example_SUITE.erl
 %%% Author  : 
 %%% Description : 
 %%%
 %%% Created : 
 %%%-------------------------------------------------------------------
 -module(groupVoiceMessagingGroupModifyVoicePortalRequest_SUITE).

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
     Phone1 = '111111',
     Phone2 = '222222',
     Grp = 'VK11111118',
     
     application:ensure_all_started(em),     
     [{grp, Grp }, {phone1, Phone1 } , {phone2, Phone2 }  | Config].
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
     Grp = ?config(grp, Config),
     ok = em_event_server:process_event(delete_grp(Grp)),
     ok.

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
     [
     modify_voiceportal_no_change,
     modify_voiceportal_set_phone,
     modify_voiceportal_update_phone,
     modify_voiceportal_phone_not_updated
     ].   
     
 %%--------------------------------------------------------------------
 %% Function: TestCase() -> Info
 %% Info = [tuple()]
 %%--------------------------------------------------------------------
 modify_voiceportal_no_change() ->
    []. 
 modify_voiceportal_set_phone() ->
    []. 
 modify_voiceportal_update_phone() ->
    []. 
 modify_voiceportal_phone_not_updated() ->
    [].     
 %%--------------------------------------------------------------------
 %% Function: TestCase(Config0) ->
 %%               ok | exit() | {skip,Reason} | {comment,Comment} |
 %%               {save_config,Config1} | {skip_and_save,Reason,Config1}
 %% Config0 = Config1 = [tuple()]
 %% Reason = term()
 %% Comment = term()
 %%--------------------------------------------------------------------
 modify_voiceportal_no_change(Config) ->     
     Grp = ?config(grp, Config),
     ok = em_event_server:process_event(modify_nil(Grp)).

 modify_voiceportal_set_phone(Config) ->     
     Grp = ?config(grp, Config),
     Phone1 = ?config(phone1, Config),
     ok = em_event_server:process_event(modify_set_phone(Grp, Phone1)).
     
 modify_voiceportal_update_phone(Config) ->     
     Grp = ?config(grp, Config),
     Phone1 = ?config(phone1, Config),
     Phone2 = ?config(phone2, Config),
     ok = em_event_server:process_event(modify_set_phone(Grp, Phone1)),
     ok = em_event_server:process_event(modify_set_phone(Grp, Phone2)).
     
 modify_voiceportal_phone_not_updated(Config) ->     
     Grp = ?config(grp, Config),
     Phone1 = ?config(phone1, Config),
     ok = em_event_server:process_event(modify_set_phone(Grp, Phone1)),
     ok = em_event_server:process_event(modify_set_phone(Grp, Phone1)).

%%%===================================================================
%%% Internal functions
%%%===================================================================
modify_nil(Grp) ->
    io_lib:format("<BroadsoftOCIReportingDocument protocol=\"OCIReporting\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\">
	<command xsi:type=\"OCIReportingReportNotification\">
		<id>write257</id>
		<userId>admin</userId>
		<loginType>System</loginType>
		<request><![CDATA[<?xml version=\"1.0\" encoding=\"ISO-8859-1\"?>
			<BroadsoftDocument protocol=\"OCI\" xmlns=\"C\">
				<userId xmlns=\"\">admin</userId>
                <command xsi:type=\"GroupVoiceMessagingGroupModifyVoicePortalRequest\" xmlns=\"\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\">
                    <serviceProviderId>SP1</serviceProviderId>
                    <groupId>~s</groupId>
                    <serviceInstanceProfile>
                        <name>Voice Portal</name>
                        <callingLineIdLastName>Voice Portal</callingLineIdLastName>
                        <callingLineIdFirstName>Voice Portal</callingLineIdFirstName>
                        <phoneNumber xsi:nil=\"true\"/>
                        <extension xsi:nil=\"true\"/>
                        <language>English</language>
                        <timeZone>Europe/Copenhagen</timeZone>
                        <sipAliasList xsi:nil=\"true\"/>
                        <publicUserIdentity>158031643_146852528_VMR@lab24.timezone4.com</publicUserIdentity>
                    </serviceInstanceProfile>
                    <isActive>false</isActive>
                    <enableExtendedScope>false</enableExtendedScope>
                    <allowIdentificationByPhoneNumberOrVoiceMailAliasesOnLogin>false</allowIdentificationByPhoneNumberOrVoiceMailAliasesOnLogin>
                    <useVoicePortalWizard>true</useVoicePortalWizard>
                    <voicePortalExternalRoutingScope>System</voicePortalExternalRoutingScope>
                    <useExternalRouting>false</useExternalRouting>
                    <externalRoutingAddress xsi:nil=\"true\"/>
                </command>
			</BroadsoftDocument>]]>
		</request>
	</command>
</BroadsoftOCIReportingDocument>",[Grp]).


modify_set_phone(Grp, Phone) ->
    io_lib:format("<BroadsoftOCIReportingDocument protocol=\"OCIReporting\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\">
	<command xsi:type=\"OCIReportingReportNotification\">
		<id>write257</id>
		<userId>admin</userId>
		<loginType>System</loginType>
		<request><![CDATA[<?xml version=\"1.0\" encoding=\"ISO-8859-1\"?>
			<BroadsoftDocument protocol=\"OCI\" xmlns=\"C\">
				<userId xmlns=\"\">admin</userId>
                <command xsi:type=\"GroupVoiceMessagingGroupModifyVoicePortalRequest\" xmlns=\"\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\">
                    <serviceProviderId>SP1</serviceProviderId>
                    <groupId>~s</groupId>
                    <serviceInstanceProfile>
                        <name>Voice Portal</name>
                        <callingLineIdLastName>Voice Portal</callingLineIdLastName>
                        <callingLineIdFirstName>Voice Portal</callingLineIdFirstName>
                        <phoneNumber>~s</phoneNumber>
                        <extension>3333</extension>
                        <language>English</language>
                        <timeZone>Europe/Copenhagen</timeZone>
                        <sipAliasList xsi:nil=\"true\"/>
                        <publicUserIdentity>158031643_146852528_VMR@lab24.timezone4.com</publicUserIdentity>
                    </serviceInstanceProfile>
                    <isActive>false</isActive>
                    <enableExtendedScope>false</enableExtendedScope>
                    <allowIdentificationByPhoneNumberOrVoiceMailAliasesOnLogin>false</allowIdentificationByPhoneNumberOrVoiceMailAliasesOnLogin>
                    <useVoicePortalWizard>true</useVoicePortalWizard>
                    <voicePortalExternalRoutingScope>System</voicePortalExternalRoutingScope>
                    <useExternalRouting>false</useExternalRouting>
                    <externalRoutingAddress xsi:nil=\"true\"/>
                </command>
			</BroadsoftDocument>]]>
		</request>
	</command>
</BroadsoftOCIReportingDocument>",[Grp, Phone]).

delete_grp(Grp) ->
    io_lib:format("<BroadsoftOCIReportingDocument protocol=\"OCIReporting\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\">
	<command xsi:type=\"OCIReportingReportNotification\">
		<id>write257</id>
		<userId>admin</userId>
		<loginType>System</loginType>
		<request><![CDATA[<?xml version=\"1.0\" encoding=\"ISO-8859-1\"?>
			<BroadsoftDocument protocol=\"OCI\" xmlns=\"C\">
				<userId xmlns=\"\">admin</userId>
                <command xsi:type=\"GroupDeleteRequest\" xmlns=\"\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\">
                    <serviceProviderId>SP1</serviceProviderId>
                    <groupId>~s</groupId>
                </command>
			</BroadsoftDocument>]]>
		</request>
	</command>
</BroadsoftOCIReportingDocument>",[Grp]).

