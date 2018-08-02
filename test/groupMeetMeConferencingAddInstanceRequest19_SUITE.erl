%%%-------------------------------------------------------------------
 %%% File    : example_SUITE.erl
 %%% Author  : 
 %%% Description : 
 %%%
 %%% Created : 
 %%%-------------------------------------------------------------------
 -module(groupMeetMeConferencingAddInstanceRequest19_SUITE).

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
     User = 'me1111@lab24.timezone4.com',
     
     application:ensure_all_started(em),
     ok = em_event_server:process_event(delete(User)),
     
     [{sp, SP},{grp, Grp},{user, User } | Config].
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
     User = ?config(user, Config),
     ok = em_event_server:process_event(delete(User)),
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
     [create_meetme].   

 %%--------------------------------------------------------------------
 %% Function: TestCase() -> Info
 %% Info = [tuple()]
 %%--------------------------------------------------------------------
create_meetme() ->
    []. 
    
 %%--------------------------------------------------------------------
 %% Function: TestCase(Config0) ->
 %%               ok | exit() | {skip,Reason} | {comment,Comment} |
 %%               {save_config,Config1} | {skip_and_save,Reason,Config1}
 %% Config0 = Config1 = [tuple()]
 %% Reason = term()
 %% Comment = term()
 %%--------------------------------------------------------------------

create_meetme(Config) ->
    SP = ?config(sp, Config),
    Grp = ?config(grp, Config),
    User = ?config(user, Config),
      
    ok = em_event_server:process_event(add(SP, Grp, User)).
    
    
add(SP, Grp, User) ->
    io_lib:format("<BroadsoftOCIReportingDocument protocol=\"OCIReporting\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\">
    <command xsi:type=\"OCIReportingReportNotification\">
        <id>write256</id>
        <userId>admin</userId>
        <loginType>System</loginType>
        <request><![CDATA[<?xml version=\"1.0\" encoding=\"ISO-8859-1\"?>
            <BroadsoftDocument protocol=\"OCI\" xmlns=\"C\">
                <userId xmlns=\"\">admin</userId>
                <command xsi:type=\"GroupMeetMeConferencingAddInstanceRequest19\" xmlns=\"\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\">
                    <serviceProviderId>~s</serviceProviderId>
                        <groupId>~s</groupId>
                        <serviceUserId>~s</serviceUserId>
                        <serviceInstanceProfile>
                            <name>aa1111</name>
                            <callingLineIdLastName>me1111</callingLineIdLastName>
                            <callingLineIdFirstName>me1111</callingLineIdFirstName>
                            <language>English</language>
                            <timeZone>Europe/Copenhagen</timeZone>
                        </serviceInstanceProfile>
                        <allocatedPorts>
                        	<quantity>2</quantity>
                        </allocatedPorts>
                        <securityPinLength>6</securityPinLength>
                        <allowIndividualOutDial>true</allowIndividualOutDial>
                        <playWarningPrompt>false</playWarningPrompt>
                        <conferenceEndWarningPromptMinutes>10</conferenceEndWarningPromptMinutes>
                        <enableMaxConferenceDuration>false</enableMaxConferenceDuration>
                        <maxConferenceDurationMinutes>
                        	<hours>3</hours>
                        	<minutes>0</minutes>
                        </maxConferenceDurationMinutes>
                        <maxScheduledConferenceDurationMinutes>
                        	<hours>23</hours>
                        	<minutes>45</minutes>
                        </maxScheduledConferenceDurationMinutes>
                </command>
            </BroadsoftDocument>]]>
        </request>
    </command>
    </BroadsoftOCIReportingDocument>",[SP, Grp, User]).
                
delete(User) ->
    io_lib:format("<BroadsoftOCIReportingDocument protocol=\"OCIReporting\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\">
	<command xsi:type=\"OCIReportingReportNotification\">
		<id>write257</id>
		<userId>admin</userId>
		<loginType>System</loginType>
		<request><![CDATA[<?xml version=\"1.0\" encoding=\"ISO-8859-1\"?>
			<BroadsoftDocument protocol=\"OCI\" xmlns=\"C\">
				<userId xmlns=\"\">admin</userId>
				<command xsi:type=\"GroupMeetMeConferencingDeleteInstanceRequest\" xmlns=\"\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\">
					<serviceUserId>~s</serviceUserId>
				</command>
			</BroadsoftDocument>]]>
		</request>
	</command>
</BroadsoftOCIReportingDocument>",[User]).

    
    
