%%%-------------------------------------------------------------------
 %%% File    : example_SUITE.erl
 %%% Author  : 
 %%% Description : 
 %%%
 %%% Created : 
 %%%-------------------------------------------------------------------
 -module(groupHuntGroupModifyInstanceRequest_SUITE).

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
     User = 'hg1111@lab24.timezone4.com',
     PubId1 = 'hgtest1@lab24.timezone4.com',
     PubId2 = 'hgtest2@lab24.timezone4.com',
     Phone1 = '333333',
     Phone2 = '444444',
     
     application:ensure_all_started(em),
     ok = em_event_server:process_event(delete(User)),
     
     [{sp, SP},{grp, Grp},{user, User },{pubid1, PubId1},{pubid2, PubId2}, {phone1, Phone1}, {phone2, Phone2} | Config].
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
     
     SP = ?config(sp, Config),
     Grp = ?config(grp, Config),
     User = ?config(user, Config),
       
     ok = em_event_server:process_event(add(SP, Grp, User)),
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
     [
     create_delete,
     modify_no_change,
     modify_set_phone,
     modify_update_phone,
     modify_delete_phone,
     modify_no_change_phone,
     modify_set_pubid_default,
     modify_set_phone_set_pubid_default,
     modify_update_phone_set_pubid_default,
     modify_delete_phone_set_pubid_default,
     modify_update_pubid_default_no_change,
     modify_set_phone_update_pubid_default_no_change,
     modify_update_phone_set_pubid_default_no_change,
     modify_delete_phone_update_pubid_default_no_change,
     modify_delete_pubid_default,
     modify_set_phone_delete_pubid_default,
     modify_update_phone_delete_pubid_default,
     modify_delete_phone_delete_pubid_default,
     modify_update_pubid_from_default,
     modify_set_phone_update_pubid_from_default,
     modify_update_phone_update_pubid_from_default,
     modify_delete_phone_update_pubid_from_default,
     modify_update_pubid_to_default,
     modify_set_phone_update_pubid_to_default,
     modify_update_phone_update_pubid_to_default,
     modify_delete_phone_update_pubid_to_default,
     modify_update_pubid,
     modify_set_phone_update_pubid,
     modify_update_phone_update_pubid,
     modify_delete_phone_update_pubid,
     modify_delete_pubid,
     modify_set_phone_delete_pubid,
     modify_update_phone_delete_pubid,
     modify_delete_phone_delete_pubid,
     modify_set_pubid,
     modify_set_phone_set_pubid,
     modify_update_phone_set_pubid,
     modify_delete_phone_set_pubid,
     modify_set_pubid_no_change,
     modify_set_phone_update_pubid_no_change,
     modify_update_phone_update_pubid_no_change,
     modify_delete_phone_update_pubid_no_change
     ].   

 %%--------------------------------------------------------------------
 %% Function: TestCase() -> Info
 %% Info = [tuple()]
 %%--------------------------------------------------------------------
create_delete() ->
    []. 
modify_no_change() ->
    [].
modify_set_phone() ->
    [].
modify_update_phone() ->
    [].
modify_delete_phone() ->
    [].  
modify_no_change_phone() ->
    [].
modify_set_pubid_default() ->
    [].
modify_set_phone_set_pubid_default() ->
    [].
modify_update_phone_set_pubid_default() ->
    [].
modify_delete_phone_set_pubid_default() ->
    [].
modify_update_pubid_default_no_change() ->
    [].
modify_set_phone_update_pubid_default_no_change() ->
    [].
modify_update_phone_set_pubid_default_no_change() ->
    [].
modify_delete_phone_update_pubid_default_no_change() ->
    [].
modify_delete_pubid_default() ->
    [].
modify_set_phone_delete_pubid_default() ->
    [].
modify_update_phone_delete_pubid_default() ->
    [].
modify_delete_phone_delete_pubid_default() ->
    [].
modify_update_pubid_from_default() ->
    [].
modify_set_phone_update_pubid_from_default() ->
    [].
modify_update_phone_update_pubid_from_default() ->
    [].
modify_delete_phone_update_pubid_from_default() ->
    [].
modify_update_pubid_to_default() ->
    [].
modify_set_phone_update_pubid_to_default() ->
    [].
modify_update_phone_update_pubid_to_default() ->
    [].
modify_delete_phone_update_pubid_to_default() ->
    [].
modify_update_pubid() ->
    [].
modify_set_phone_update_pubid() ->
    [].
modify_update_phone_update_pubid() ->
    [].
modify_delete_phone_update_pubid() ->
    [].
modify_delete_pubid() ->
    [].
modify_set_phone_delete_pubid() ->
    [].
modify_update_phone_delete_pubid() ->
    [].
modify_delete_phone_delete_pubid() ->
    [].
modify_set_pubid() ->
    [].
modify_set_phone_set_pubid() ->
    [].
modify_update_phone_set_pubid() ->
    [].
modify_delete_phone_set_pubid() ->
    [].
modify_set_pubid_no_change() ->
    [].
modify_set_phone_update_pubid_no_change() ->
    [].
modify_update_phone_update_pubid_no_change() ->
    [].
modify_delete_phone_update_pubid_no_change() ->
    [].
    
 %%--------------------------------------------------------------------
 %% Function: TestCase(Config0) ->
 %%               ok | exit() | {skip,Reason} | {comment,Comment} |
 %%               {save_config,Config1} | {skip_and_save,Reason,Config1}
 %% Config0 = Config1 = [tuple()]
 %% Reason = term()
 %% Comment = term()
 %%--------------------------------------------------------------------

 %%--------------------------------------------------------------------
 %% PublicId 
 %% States: nil, default, non-default
 %%
 %% State transitions:
 %% T1: nil         -> nil
 %% T2: nil         -> default
 %% T3: default     -> default
 %% T4: default     -> nil
 %% T5: default     -> non-default
 %% T6: non-default -> default
 %% T7: non-default -> non-default (new value) + (same value)
 %% T8: non-default -> nil
 %% T9: nil         -> non-default
 %%
 %% Phone 
 %% States: nil, set
 %% State transitions: 
 %% T1: nil -> nil
 %% T2: nil -> set
 %% T3: set -> set (new value) + (same value)
 %% T4: set -> nil

 
 %%--------------------------------------------------------------------

create_delete(_Config) ->
    ok.

% Block 1    
% PHONE-T1 + PUBID-T1
modify_no_change(Config) ->
    User = ?config(user, Config),
    ok = em_event_server:process_event(nil_nil(User)).

% PHONE-T2 + PUBID-T1 
modify_set_phone(Config) ->
    User = ?config(user, Config),
    Phone1 = ?config(phone1, Config),
    ok = em_event_server:process_event(set_nil(User, Phone1)).

% PHONE-T3 + PUBID-T1
modify_update_phone(Config) ->
    User = ?config(user, Config),
    Phone1 = ?config(phone1, Config),
    Phone2 = ?config(phone2, Config),
    ok = em_event_server:process_event(set_nil(User, Phone1)),
    ok = em_event_server:process_event(set_nil(User, Phone2)).    

% PHONE-T4 + PUBID-T1 
modify_delete_phone(Config) ->
    User = ?config(user, Config),
    Phone1 = ?config(phone1, Config),
    ok = em_event_server:process_event(set_nil(User, Phone1)),
    ok = em_event_server:process_event(nil_nil(User)).

% PHONE-T5 + PUBID-T1    
modify_no_change_phone(Config) ->
    User = ?config(user, Config),
    Phone1 = ?config(phone1, Config),
    ok = em_event_server:process_event(set_nil(User, Phone1)),
    ok = em_event_server:process_event(set_nil(User, Phone1)).


% Block 2  
% PHONE-T1 + PUBID-T2 
modify_set_pubid_default(Config) ->
    User= ?config(user, Config),
    ok = em_event_server:process_event(nil_set(User, User)).

% PHONE-T2 + PUBID-T2 
modify_set_phone_set_pubid_default(Config) ->
    Phone1 = ?config(phone1, Config),
    User= ?config(user, Config),
    ok = em_event_server:process_event(set_set(User, Phone1, User)).

% PHONE-T3 + PUBID-T2 
modify_update_phone_set_pubid_default(Config) ->
    Phone1 = ?config(phone1, Config),
    Phone2 = ?config(phone2, Config),
    User= ?config(user, Config),
    ok = em_event_server:process_event(set_nil(User, Phone1)),
    ok = em_event_server:process_event(set_set(User, Phone2, User)).

% PHONE-T4 + PUBID-T2 
modify_delete_phone_set_pubid_default(Config) ->
    Phone1 = ?config(phone1, Config),
    User= ?config(user, Config),
    ok = em_event_server:process_event(set_nil(User, Phone1)),
    ok = em_event_server:process_event(nil_set(User, User)).


% Block 3  
% PHONE-T1 + PUBID-T3 
modify_update_pubid_default_no_change(Config) ->
    User= ?config(user, Config),
    ok = em_event_server:process_event(nil_set(User, User)),
    ok = em_event_server:process_event(nil_set(User, User)).



% PHONE-T2 + PUBID-T3 
modify_set_phone_update_pubid_default_no_change(Config) ->
    Phone1 = ?config(phone1, Config),
    User= ?config(user, Config),
    ok = em_event_server:process_event(nil_set(User, User)),
    ok = em_event_server:process_event(set_set(User, Phone1, User)).

% PHONE-T3 + PUBID-T3 
modify_update_phone_set_pubid_default_no_change(Config) ->
    Phone1 = ?config(phone1, Config),
    Phone2 = ?config(phone2, Config),
    User= ?config(user, Config),
    ok = em_event_server:process_event(set_set(User, Phone1, User)),
    ok = em_event_server:process_event(set_set(User, Phone2, User)).
    
% PHONE-T4 + PUBID-T3 
modify_delete_phone_update_pubid_default_no_change(Config) ->
    Phone1 = ?config(phone1, Config),
    User= ?config(user, Config),
    ok = em_event_server:process_event(set_set(User, Phone1, User)),
    ok = em_event_server:process_event(nil_set(User, User)).

    
% Block 4  
% PHONE-T1 + PUBID-T4 
modify_delete_pubid_default(Config) ->
    User= ?config(user, Config),
    ok = em_event_server:process_event(nil_set(User, User)),
    ok = em_event_server:process_event(nil_nil(User)).
    
% PHONE-T2 + PUBID-T4 
modify_set_phone_delete_pubid_default(Config) ->
    Phone1 = ?config(phone1, Config),
    User= ?config(user, Config),
    ok = em_event_server:process_event(nil_set(User, User)),
    ok = em_event_server:process_event(set_nil(User, Phone1)).
    
% PHONE-T3 + PUBID-T4 
modify_update_phone_delete_pubid_default(Config) ->
    Phone1 = ?config(phone1, Config),
    Phone2 = ?config(phone2, Config),
    User= ?config(user, Config),
    ok = em_event_server:process_event(set_set(User, Phone1, User)),
    ok = em_event_server:process_event(set_nil(User, Phone2)).
    
% PHONE-T4 + PUBID-T4 
modify_delete_phone_delete_pubid_default(Config) ->
    Phone1 = ?config(phone1, Config),
    User= ?config(user, Config),
    ok = em_event_server:process_event(set_set(User, Phone1, User)),
    ok = em_event_server:process_event(nil_nil(User)).


% Block 5
% PHONE-T1 + PUBID-T5 
modify_update_pubid_from_default(Config) ->
    User= ?config(user, Config),
    PubId1 = '1111aa@lab24.timezone4.com',
    ok = em_event_server:process_event(nil_set(User, User)),
    ok = em_event_server:process_event(nil_set(User, PubId1)).
    
modify_set_phone_update_pubid_from_default(Config) ->
    Phone1 = ?config(phone1, Config),
    User = ?config(user, Config),
    PubId1 = ?config(pubid1, Config),
    ok = em_event_server:process_event(nil_set(User, User)),
    ok = em_event_server:process_event(set_set(User, Phone1, PubId1)). 

% PHONE-T3 + PUBID-T5 
modify_update_phone_update_pubid_from_default(Config) ->
    Phone1 = ?config(phone1, Config),
    Phone2 = ?config(phone2, Config),
    User = ?config(user, Config),
    PubId1 = ?config(pubid1, Config),
    ok = em_event_server:process_event(set_set(User, Phone1, User)),
    ok = em_event_server:process_event(set_set(User, Phone2, PubId1)).
    
    
% PHONE-T4 + PUBID-T5 
modify_delete_phone_update_pubid_from_default(Config) ->
    Phone1 = ?config(phone1, Config),
    User = ?config(user, Config),
    PubId1 = ?config(pubid1, Config),
    ok = em_event_server:process_event(set_set(User, Phone1, User)),
    ok = em_event_server:process_event(nil_set(User, PubId1)).

% Block 6
% PHONE-T1 + PUBID-T6
modify_update_pubid_to_default(Config) ->
    PubId1 = ?config(pubid1, Config),
    User = ?config(user, Config),
    ok = em_event_server:process_event(nil_set(User, PubId1)),
    ok = em_event_server:process_event(nil_set(User, User)).
     
% PHONE-T2 + PUBID-T6
modify_set_phone_update_pubid_to_default(Config) ->
    Phone1 = ?config(phone1, Config),
    PubId1 = ?config(pubid1, Config),
    User = ?config(user, Config),
    ok = em_event_server:process_event(nil_set(User, PubId1)),
    ok = em_event_server:process_event(set_set(User, Phone1, User)).
     
% PHONE-T3 + PUBID-T6 
modify_update_phone_update_pubid_to_default(Config) ->
    Phone1 = ?config(phone1, Config),
    Phone2 = ?config(phone2, Config),
    PubId1 = ?config(pubid1, Config),
    User = ?config(user, Config),
    ok = em_event_server:process_event(set_set(User, Phone1, PubId1)),
    ok = em_event_server:process_event(set_set(User, Phone2, User)).
    
% PHONE-T4 + PUBID-T6 
modify_delete_phone_update_pubid_to_default(Config) ->
    Phone1 = ?config(phone1, Config),
    PubId1 = ?config(pubid1, Config),
    User = ?config(user, Config),
    ok = em_event_server:process_event(set_set(User, Phone1, PubId1)),
    ok = em_event_server:process_event(nil_set(User, User)).
    
% Block 7
% PHONE-T1 + PUBID-T7 
modify_update_pubid(Config) ->
    User = ?config(user, Config),
    PubId1 = ?config(pubid1, Config),
    PubId2 = ?config(pubid2, Config),
    ok = em_event_server:process_event(nil_set(User, PubId1)),
    ok = em_event_server:process_event(nil_set(User, PubId2)).
    
% PHONE-T2 + PUBID-T7 
modify_set_phone_update_pubid(Config) ->
    User = ?config(user, Config),
    Phone1 = ?config(phone1, Config),
    PubId1 = ?config(pubid1, Config),
    PubId2 = ?config(pubid2, Config),
    ok = em_event_server:process_event(nil_set(User, PubId1)),
    ok = em_event_server:process_event(set_set(User, Phone1, PubId2)).
    
% PHONE-T3 + PUBID-T7 
modify_update_phone_update_pubid(Config) ->
    User = ?config(user, Config),
    Phone1 = ?config(phone1, Config),
    Phone2 = ?config(phone2, Config),
    PubId1 = ?config(pubid1, Config),
    PubId2 = ?config(pubid2, Config),
    ok = em_event_server:process_event(set_set(User, Phone1, PubId1)),
    ok = em_event_server:process_event(set_set(User, Phone2, PubId2)).
    
% PHONE-T4 + PUBID-T7
modify_delete_phone_update_pubid(Config) ->
    User = ?config(user, Config),
    Phone1 = ?config(phone1, Config),
    PubId1 = ?config(pubid1, Config),
    PubId2 = ?config(pubid2, Config),
    ok = em_event_server:process_event(set_set(User, Phone1, PubId1)),
    ok = em_event_server:process_event(nil_set(User, PubId2)).
    
% Block 8
% PHONE-T1 + PUBID-T8 
modify_delete_pubid(Config) ->
    User = ?config(user, Config),
    PubId1 = ?config(pubid1, Config),
    ok = em_event_server:process_event(nil_set(User, PubId1)),
    ok = em_event_server:process_event(nil_nil(User)).

% PHONE-T2 + PUBID-T8 
modify_set_phone_delete_pubid(Config) ->
    User = ?config(user, Config),
    Phone1 = ?config(phone1, Config),
    PubId1 = ?config(pubid1, Config),
    ok = em_event_server:process_event(nil_set(User, PubId1)),
    ok = em_event_server:process_event(set_nil(User, Phone1)).

% PHONE-T3 + PUBID-T8 
modify_update_phone_delete_pubid(Config) ->
    User = ?config(user, Config),
    Phone1 = ?config(phone1, Config),
    Phone2 = ?config(phone2, Config),
    PubId1 = ?config(pubid1, Config),
    ok = em_event_server:process_event(set_set(User, Phone1, PubId1)),
    ok = em_event_server:process_event(set_nil(User, Phone2)).
    
% PHONE-T4 + PUBID-T8
modify_delete_phone_delete_pubid(Config) ->
    User = ?config(user, Config),
    Phone1 = ?config(phone1, Config),
    PubId1 = ?config(pubid1, Config),
    ok = em_event_server:process_event(set_set(User,Phone1, PubId1)),
    ok = em_event_server:process_event(nil_nil(User)).
    
% Block 9
% PHONE-T1 + PUBID-T9 
modify_set_pubid(Config) ->
    User = ?config(user, Config),
    PubId1 = ?config(pubid1, Config),
    ok = em_event_server:process_event(nil_set(User, PubId1)).
    
% PHONE-T2 + PUBID-T9 
modify_set_phone_set_pubid(Config) ->
    User = ?config(user, Config),
    Phone1 = ?config(phone1, Config),
    PubId1 = ?config(pubid1, Config),
    ok = em_event_server:process_event(set_set(User, Phone1, PubId1)).
    
% Test case 35: PHONE-T3 + PUBID-T9 
modify_update_phone_set_pubid(Config) ->
    User = ?config(user, Config),
    Phone1 = ?config(phone1, Config),
    Phone2 = ?config(phone2, Config),
    PubId1 = ?config(pubid1, Config),
    ok = em_event_server:process_event(set_nil(User, Phone1)),
    ok = em_event_server:process_event(set_set(User, Phone2, PubId1)).
    
% PHONE-T4 + PUBID-T9 
modify_delete_phone_set_pubid(Config) ->
    User = ?config(user, Config),
    Phone1 = ?config(phone1, Config),
    PubId1 = ?config(pubid1, Config),
    ok = em_event_server:process_event(set_nil(User, Phone1)),
    ok = em_event_server:process_event(nil_set(User, PubId1)). 
 

% Block 10
% PHONE-T1 + PUBID-T7 (same value)
modify_set_pubid_no_change(Config) ->
    User = ?config(user, Config),
    PubId1 = ?config(pubid1, Config),
    ok = em_event_server:process_event(nil_set(User, PubId1)),
    ok = em_event_server:process_event(nil_set(User, PubId1)).
    
% PHONE-T2 + PUBID-T7 (same value)
modify_set_phone_update_pubid_no_change(Config) ->
    User = ?config(user, Config),
    Phone1 = ?config(phone1, Config),
    PubId1 = ?config(pubid1, Config),
    ok = em_event_server:process_event(nil_set(User,PubId1)),
    ok = em_event_server:process_event(set_set(User,Phone1, PubId1)).
    
% PHONE-T3 + PUBID-T7 (same value)
modify_update_phone_update_pubid_no_change(Config) ->
    User = ?config(user, Config),
    Phone1 = ?config(phone1, Config),
    Phone2 = ?config(phone2, Config),
    PubId1 = ?config(pubid1, Config),
    ok = em_event_server:process_event(set_set(User, Phone1, PubId1)),
    ok = em_event_server:process_event(set_set(User, Phone2, PubId1)).
    
% PHONE-T4 + PUBID-T7 (same value)
modify_delete_phone_update_pubid_no_change(Config) ->
    User = ?config(user, Config),
    Phone1 = ?config(phone1, Config),
    PubId1 = ?config(pubid1, Config),
    ok = em_event_server:process_event(set_set(User, Phone1, PubId1)),
    ok = em_event_server:process_event(nil_set(User,PubId1)).
    

add(SP, Grp, User) ->
    io_lib:format("<BroadsoftOCIReportingDocument protocol=\"OCIReporting\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\">
    <command xsi:type=\"OCIReportingReportNotification\">
        <id>write256</id>
        <userId>admin</userId>
        <loginType>System</loginType>
        <request><![CDATA[<?xml version=\"1.0\" encoding=\"ISO-8859-1\"?>
            <BroadsoftDocument protocol=\"OCI\" xmlns=\"C\">
                <userId xmlns=\"\">admin</userId>
                <command xsi:type=\"GroupHuntGroupAddInstanceRequest20\" xmlns=\"\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\">
                    <serviceProviderId>~s</serviceProviderId>
                        <groupId>~s</groupId>
                        <serviceUserId>~s</serviceUserId>
                        <serviceInstanceProfile>
                            <name>aa1111</name>
                            <callingLineIdLastName>aa1111</callingLineIdLastName>
                            <callingLineIdFirstName>aa1111</callingLineIdFirstName>
                            <language>English</language>
                            <timeZone>Europe/Copenhagen</timeZone>
                        </serviceInstanceProfile>
                        <policy>Regular</policy>
					    <huntAfterNoAnswer>false</huntAfterNoAnswer>
					    <noAnswerNumberOfRings>5</noAnswerNumberOfRings>
					    <forwardAfterTimeout>false</forwardAfterTimeout>
					    <forwardTimeoutSeconds>0</forwardTimeoutSeconds>
					    <allowCallWaitingForAgents>false</allowCallWaitingForAgents>
					    <useSystemHuntGroupCLIDSetting>true</useSystemHuntGroupCLIDSetting>
					    <includeHuntGroupNameInCLID>true</includeHuntGroupNameInCLID>
					    <enableNotReachableForwarding>false</enableNotReachableForwarding>
					    <makeBusyWhenNotReachable>false</makeBusyWhenNotReachable>
					    <allowMembersToControlGroupBusy>false</allowMembersToControlGroupBusy>
					    <enableGroupBusy>false</enableGroupBusy>
					    <applyGroupBusyWhenTerminatingToAgent>false</applyGroupBusyWhenTerminatingToAgent>
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
				<command xsi:type=\"GroupHuntGroupDeleteInstanceRequest\" xmlns=\"\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\">
					<serviceUserId>~s</serviceUserId>
				</command>
			</BroadsoftDocument>]]>
		</request>
	</command>
</BroadsoftOCIReportingDocument>",[User]).

nil_nil(User) ->
    io_lib:format("<BroadsoftOCIReportingDocument protocol=\"OCIReporting\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\">
	<command xsi:type=\"OCIReportingReportNotification\">
		<id>write262</id>
		<userId>admin</userId>
		<loginType>System</loginType>
		<request><![CDATA[<?xml version=\"1.0\" encoding=\"ISO-8859-1\"?>
			<BroadsoftDocument protocol=\"OCI\" xmlns=\"C\">
				<userId xmlns=\"\">admin</userId>
					<command xsi:type=\"GroupHuntGroupModifyInstanceRequest\" xmlns=\"\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\">
						<serviceUserId>~s</serviceUserId>
						<serviceInstanceProfile>
							<phoneNumber xsi:nil=\"true\"/>
							<extension>6666</extension>
							<sipAliasList xsi:nil=\"true\"/>
							<publicUserIdentity xsi:nil=\"true\"/>
						</serviceInstanceProfile>
					</command>
			</BroadsoftDocument>]]>
		</request>
	</command>
</BroadsoftOCIReportingDocument>",[User]).    

set_nil(User, Phone) ->
    io_lib:format("<BroadsoftOCIReportingDocument protocol=\"OCIReporting\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\">
	<command xsi:type=\"OCIReportingReportNotification\">
		<id>write262</id>
		<userId>admin</userId>
		<loginType>System</loginType>
		<request><![CDATA[<?xml version=\"1.0\" encoding=\"ISO-8859-1\"?>
			<BroadsoftDocument protocol=\"OCI\" xmlns=\"C\">
				<userId xmlns=\"\">admin</userId>
					<command xsi:type=\"GroupHuntGroupModifyInstanceRequest\" xmlns=\"\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\">
						<serviceUserId>~s</serviceUserId>
						<serviceInstanceProfile>
							<phoneNumber>~s</phoneNumber>
							<extension>6666</extension>
							<sipAliasList xsi:nil=\"true\"/>
							<publicUserIdentity xsi:nil=\"true\"/>
						</serviceInstanceProfile>
					</command>
			</BroadsoftDocument>]]>
		</request>
	</command>
</BroadsoftOCIReportingDocument>",[User, Phone]).

nil_set(User, PubId) ->
    io_lib:format("<BroadsoftOCIReportingDocument protocol=\"OCIReporting\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\">
	<command xsi:type=\"OCIReportingReportNotification\">
		<id>write262</id>
		<userId>admin</userId>
		<loginType>System</loginType>
		<request><![CDATA[<?xml version=\"1.0\" encoding=\"ISO-8859-1\"?>
			<BroadsoftDocument protocol=\"OCI\" xmlns=\"C\">
				<userId xmlns=\"\">admin</userId>
					<command xsi:type=\"GroupHuntGroupModifyInstanceRequest\" xmlns=\"\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\">
						<serviceUserId>~s</serviceUserId>
						<serviceInstanceProfile>
							<phoneNumber xsi:nil=\"true\"/>
							<extension>6666</extension>
							<sipAliasList xsi:nil=\"true\"/>
                            <publicUserIdentity>~s</publicUserIdentity>
						</serviceInstanceProfile>
					</command>
			</BroadsoftDocument>]]>
		</request>
	</command>
</BroadsoftOCIReportingDocument>",[User, PubId]).


set_set(User, Phone, PubId) ->
    io_lib:format("<BroadsoftOCIReportingDocument protocol=\"OCIReporting\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\">
	<command xsi:type=\"OCIReportingReportNotification\">
		<id>write262</id>
		<userId>admin</userId>
		<loginType>System</loginType>
		<request><![CDATA[<?xml version=\"1.0\" encoding=\"ISO-8859-1\"?>
			<BroadsoftDocument protocol=\"OCI\" xmlns=\"C\">
				<userId xmlns=\"\">admin</userId>
					<command xsi:type=\"GroupHuntGroupModifyInstanceRequest\" xmlns=\"\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\">
						<serviceUserId>~s</serviceUserId>
						<serviceInstanceProfile>
							<phoneNumber>~s</phoneNumber>
							<extension>6666</extension>
							<sipAliasList xsi:nil=\"true\"/>
                            <publicUserIdentity>~s</publicUserIdentity>
						</serviceInstanceProfile>
					</command>
			</BroadsoftDocument>]]>
		</request>
	</command>
</BroadsoftOCIReportingDocument>",[User, Phone, PubId]).
    