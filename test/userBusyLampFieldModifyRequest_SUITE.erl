%%%-------------------------------------------------------------------
 %%% File    : example_SUITE.erl
 %%% Author  : 
 %%% Description : 
 %%%
 %%% Created : 
 %%%-------------------------------------------------------------------
 -module(userBusyLampFieldModifyRequest_SUITE).

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
     User = 'blf_test_user010@lab24.timezone4.com',
     
     Uri1 = 'blf_user010@lab24.timezone4.com',
     Uri2 = 'blfuser010@lab24.timezone4.com',
     
     application:ensure_all_started(em),
     ok = em_event_server:process_event(delete(User)),
     ok = em_event_server:process_event(add(SP, Grp, User)),
          
     [{sp, SP},{grp, Grp},{user, User }, {uri1, Uri1 } , {uri2, Uri2 }  | Config].
 %%--------------------------------------------------------------------
 %% Function: end_per_suite(Config0) -> term() | {save_config,Config1}
 %% Config0 = Config1 = [tuple()]
 %%--------------------------------------------------------------------
 end_per_suite(Config) ->
    User = ?config(user, Config),
    ok = em_event_server:process_event(delete(User)),
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
     %Grp = ?config(grp, Config),
     %ok = em_event_server:process_event(delete_grp(Grp)),
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
     modify_blf_no_change,
     modify_blf_set_uri,
     modify_blf_delete_uri,
     modify_blf_update_uri,
     modify_blf_uri_not_updated,
     modify_blf_uri_changed_to_user,
     modify_blf_uri_to_user
     ].   
     
 %%--------------------------------------------------------------------
 %% Function: TestCase() -> Info
 %% Info = [tuple()]
 %%--------------------------------------------------------------------
 modify_blf_no_change() ->
    []. 
 modify_blf_set_uri() ->
    []. 
 modify_blf_delete_uri() ->
    [].    
 modify_blf_update_uri() ->
    []. 
 modify_blf_uri_not_updated() ->
    [].
 modify_blf_uri_changed_to_user() ->
    [].     
 modify_blf_uri_to_user() ->
    [].     

 %%--------------------------------------------------------------------
 %% Function: TestCase(Config0) ->
 %%               ok | exit() | {skip,Reason} | {comment,Comment} |
 %%               {save_config,Config1} | {skip_and_save,Reason,Config1}
 %% Config0 = Config1 = [tuple()]
 %% Reason = term()
 %% Comment = term()
 %%--------------------------------------------------------------------
 modify_blf_no_change(Config) ->     
     User = ?config(user, Config),
     ok = em_event_server:process_event(modify_nil(User)).

 modify_blf_set_uri(Config) ->     
     User = ?config(user, Config),
     Uri1 = ?config(uri1, Config),
     ok = em_event_server:process_event(modify_set_uri(User, Uri1)).
 
modify_blf_delete_uri(Config) ->     
     User = ?config(user, Config),
     Uri1 = ?config(uri1, Config),
     ok = em_event_server:process_event(modify_set_uri(User, Uri1)),
     ok = em_event_server:process_event(modify_nil(User)).
 
 modify_blf_update_uri(Config) ->     
     User = ?config(user, Config),
     Uri1 = ?config(uri1, Config),
     Uri2 = ?config(uri2, Config),
     ok = em_event_server:process_event(modify_set_uri(User, Uri1)),
     ok = em_event_server:process_event(modify_set_uri(User, Uri2)).
     
 modify_blf_uri_not_updated(Config) ->     
     User = ?config(user, Config),
     Uri1 = ?config(uri1, Config),
     ok = em_event_server:process_event(modify_set_uri(User, Uri1)),
     ok = em_event_server:process_event(modify_set_uri(User, Uri1)).

 modify_blf_uri_changed_to_user(Config) ->     
     User = ?config(user, Config),
     Uri1 = ?config(uri1, Config),
     ok = em_event_server:process_event(modify_set_uri(User, Uri1)),
     ok = em_event_server:process_event(modify_set_uri(User, User)).

 modify_blf_uri_to_user(Config) ->     
     User = ?config(user, Config),
     ok = em_event_server:process_event(modify_set_uri(User, User)).


%%%===================================================================
%%% Internal functions
%%%===================================================================
modify_nil(User) ->
io_lib:format("<BroadsoftOCIReportingDocument xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" protocol=\"OCIReporting\">
    <command xsi:type=\"OCIReportingReportNotification\">
        <id>write247</id>
        <userId>admin</userId>
        <loginType>System</loginType>
        <request><![CDATA[<?xml version=\"1.0\" encoding=\"ISO-8859-1\"?>
            <BroadsoftDocument protocol=\"OCI\" xmlns=\"C\">
                <userId xmlns=\"\">admin</userId>
                <command xsi:type=\"UserBusyLampFieldModifyRequest\" xmlns=\"\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\">
                    <userId>~s</userId>
                    <listURI xsi:nil=\"true\"/>
                    <monitoredUserIdList>
                        <userId>390631@ims2350.iserfik.gl</userId>
                        <userId>390632@ims2350.iserfik.gl</userId>
                        <userId>390633@ims2350.iserfik.gl</userId>
                        <userId>390634@ims2350.iserfik.gl</userId>
                    </monitoredUserIdList>
                    <enableCallParkNotification>false</enableCallParkNotification>
                </command>
            </BroadsoftDocument>]]>
        </request>
    </command>
</BroadsoftOCIReportingDocument>",[User]).


modify_set_uri(User, Uri) ->
io_lib:format("<BroadsoftOCIReportingDocument xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" protocol=\"OCIReporting\">
    <command xsi:type=\"OCIReportingReportNotification\">
        <id>write247</id>
        <userId>admin</userId>
        <loginType>System</loginType>
        <request><![CDATA[<?xml version=\"1.0\" encoding=\"ISO-8859-1\"?>
            <BroadsoftDocument protocol=\"OCI\" xmlns=\"C\">
                <userId xmlns=\"\">admin</userId>
                <command xsi:type=\"UserBusyLampFieldModifyRequest\" xmlns=\"\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\">
                    <userId>~s</userId>
                    <listURI>~s</listURI>
                    <monitoredUserIdList>
                        <userId>390631@ims2350.iserfik.gl</userId>
                        <userId>390632@ims2350.iserfik.gl</userId>
                        <userId>390633@ims2350.iserfik.gl</userId>
                        <userId>390634@ims2350.iserfik.gl</userId>
                    </monitoredUserIdList>
                    <enableCallParkNotification>false</enableCallParkNotification>
                </command>
            </BroadsoftDocument>]]>
        </request>
    </command>
</BroadsoftOCIReportingDocument>",[User, Uri]).

add(SP, Grp, User) ->
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

delete(User) ->
    io_lib:format("<BroadsoftOCIReportingDocument protocol=\"OCIReporting\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\">
    <command xsi:type=\"OCIReportingReportNotification\">
        <id>write257</id>
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




