%%%-------------------------------------------------------------------
 %%% File    : example_SUITE.erl
 %%% Author  : 
 %%% Description : 
 %%%
 %%% Created : 
 %%%-------------------------------------------------------------------
 -module(systemDomainDeleteRequest_SUITE).

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
     Domain = 'lab99.timezone4.com',
     
     application:ensure_all_started(em),     
     [{domain, Domain } | Config].
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
     [delete_domain].   
     
 %%--------------------------------------------------------------------
 %% Function: TestCase() -> Info
 %% Info = [tuple()]
 %%--------------------------------------------------------------------
 delete_domain() ->
    []. 
    
 %%--------------------------------------------------------------------
 %% Function: TestCase(Config0) ->
 %%               ok | exit() | {skip,Reason} | {comment,Comment} |
 %%               {save_config,Config1} | {skip_and_save,Reason,Config1}
 %% Config0 = Config1 = [tuple()]
 %% Reason = term()
 %% Comment = term()
 %%--------------------------------------------------------------------
 delete_domain(Config) ->     
     Domain = ?config(domain, Config),
     ok = em_event_server:process_event(add(Domain)),
     ok = em_event_server:process_event(delete(Domain)).



%%%===================================================================
%%% Internal functions
%%%===================================================================
add(Domain) ->
    io_lib:format("<BroadsoftOCIReportingDocument protocol=\"OCIReporting\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\">
	<command xsi:type=\"OCIReportingReportNotification\">
		<id>write257</id>
		<userId>admin</userId>
		<loginType>System</loginType>
		<request><![CDATA[<?xml version=\"1.0\" encoding=\"ISO-8859-1\"?>
			<BroadsoftDocument protocol=\"OCI\" xmlns=\"C\">
				<userId xmlns=\"\">admin</userId>
                <command xsi:type=\"SystemDomainAddRequest\" xmlns=\"\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\">
                    <domain>~s</domain>
                </command>
			</BroadsoftDocument>]]>
		</request>
	</command>
</BroadsoftOCIReportingDocument>",[Domain]).


delete(Domain) ->
    io_lib:format("<BroadsoftOCIReportingDocument protocol=\"OCIReporting\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\">
	<command xsi:type=\"OCIReportingReportNotification\">
		<id>write257</id>
		<userId>admin</userId>
		<loginType>System</loginType>
		<request><![CDATA[<?xml version=\"1.0\" encoding=\"ISO-8859-1\"?>
			<BroadsoftDocument protocol=\"OCI\" xmlns=\"C\">
				<userId xmlns=\"\">admin</userId>
                <command xsi:type=\"SystemDomainDeleteRequest\" xmlns=\"\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\">
                    <domain>~s</domain>
                </command>
			</BroadsoftDocument>]]>
		</request>
	</command>
</BroadsoftOCIReportingDocument>",[Domain]).


