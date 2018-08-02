%%%-------------------------------------------------------------------
 %%% File    : example_SUITE.erl
 %%% Author  : 
 %%% Description : 
 %%%
 %%% Created : 
 %%%-------------------------------------------------------------------
 -module(groupDeleteRequest_SUITE).

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
     Grp = 'VK11111115',
     User1 = 'grpuser008@lab99.timezone4.com',
     User2 = 'grpuser009@lab99.timezone4.com',
     
     application:ensure_all_started(em),     
     [{grp, Grp }, {user1, User1 } ,{user2, User2 }  | Config].
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
 end_per_testcase(_TestCase, _Config) ->
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
     [delete_group,
     delete_empty_group].   
     
 %%--------------------------------------------------------------------
 %% Function: TestCase() -> Info
 %% Info = [tuple()]
 %%--------------------------------------------------------------------
 delete_group() ->
    []. 
 delete_empty_group() ->
    [].
    
 %%--------------------------------------------------------------------
 %% Function: TestCase(Config0) ->
 %%               ok | exit() | {skip,Reason} | {comment,Comment} |
 %%               {save_config,Config1} | {skip_and_save,Reason,Config1}
 %% Config0 = Config1 = [tuple()]
 %% Reason = term()
 %% Comment = term()
 %%--------------------------------------------------------------------

% delete group with multiple users assigned
 delete_group(Config) ->     
     Grp = ?config(grp, Config),
     User1 = ?config(user1, Config),
     User2 = ?config(user2, Config),
     SP = 'SP1',
     
     ok = em_event_server:process_event(add(Grp)),
     ok = em_event_server:process_event(user_add(SP, Grp, User1)),
     ok = em_event_server:process_event(user_add(SP, Grp, User2)),
     ok = em_event_server:process_event(delete(Grp)).

 delete_empty_group(Config) ->     
     Grp = ?config(grp, Config),     
     ok = em_event_server:process_event(add(Grp)),
     ok = em_event_server:process_event(delete(Grp)).


%%%===================================================================
%%% Internal functions
%%%===================================================================
add(Grp) ->
    io_lib:format("<BroadsoftOCIReportingDocument protocol=\"OCIReporting\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\">
	<command xsi:type=\"OCIReportingReportNotification\">
		<id>write257</id>
		<userId>admin</userId>
		<loginType>System</loginType>
		<request><![CDATA[<?xml version=\"1.0\" encoding=\"ISO-8859-1\"?>
			<BroadsoftDocument protocol=\"OCI\" xmlns=\"C\">
				<userId xmlns=\"\">admin</userId>
                <command xsi:type=\"GroupAddRequest\" xmlns=\"\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\">
                    <serviceProviderId>SP1</serviceProviderId>
                    <groupId>~s</groupId>
                </command>
			</BroadsoftDocument>]]>
		</request>
	</command>
</BroadsoftOCIReportingDocument>",[Grp]).

delete(Grp) ->
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

user_add(SP, Grp, User) ->
    io_lib:format("<BroadsoftOCIReportingDocument protocol=\"OCIReporting\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\">
	<command xsi:type=\"OCIReportingReportNotification\">
		<id>write264</id>
		<userId>admin</userId>
		<loginType>System</loginType>
		<request><![CDATA[<?xml version=\"1.0\" encoding=\"ISO-8859-1\"?>
			<BroadsoftDocument protocol=\"OCI\" xmlns=\"C\">
				<userId xmlns=\"\">admin</userId>
				<command xsi:type=\"UserAddRequest17sp4\" xmlns=\"\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\">
					<serviceProviderId>~s</serviceProviderId>
					<groupId>~s</groupId>
					<userId>~s</userId>
					<lastName>Test</lastName>
					<firstName>Test</firstName>
					<callingLineIdLastName>Test</callingLineIdLastName>
					<callingLineIdFirstName>Test</callingLineIdFirstName>
					<password>123456</password>
					<language>English</language>
					<timeZone>Europe/Copenhagen</timeZone>
					<address/>
				</command>
			</BroadsoftDocument>]]>
		</request>
	</command>
</BroadsoftOCIReportingDocument>",[SP, Grp, User]).
