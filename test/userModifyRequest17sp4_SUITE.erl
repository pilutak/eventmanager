%%%-------------------------------------------------------------------
 %%% File    : example_SUITE.erl
 %%% Author  : 
 %%% Description : 
 %%%
 %%% Created : 
 %%%-------------------------------------------------------------------
 -module(userModifyRequest17sp4_SUITE).

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
     User = 'moduser010@lab24.timezone4.com',
     PubId1 = 'modusertest3@lab24.timezone4.com',
     PubId2 = 'modusertest4@lab24.timezone4.com',
     Phone1 = '888888',
     Phone2 = '999999',
     
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
     modify_delete_phone_update_pubid_no_change,
     modify_profile_city,
     modify_profile_delete_city,
     modify_user_default_pubid_to_trunkuser,
     modify_user_pubid_to_trunkuser,
     modify_trunkuser_pubid,
     modify_trunkuser_phone,
     modify_trunkuser_to_user,
     modify_nonexisting_user
     ].   
     
 %%--------------------------------------------------------------------
 %% Function: TestCase() -> Info
 %% Info = [tuple()]
 %%--------------------------------------------------------------------
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
modify_profile_city() ->
    [].
modify_profile_delete_city() ->
    [].
modify_user_default_pubid_to_trunkuser() ->
    [].
modify_user_pubid_to_trunkuser() ->
    [].
modify_trunkuser_pubid() ->
    [].
modify_trunkuser_phone() ->
    [].
modify_trunkuser_to_user() ->
    [].  
modify_nonexisting_user() ->
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
    
% Set 112 city
modify_profile_city(Config) ->
    User = ?config(user, Config),
    ok = em_event_server:process_event(city112(User, 'Nuuk')).

% Delete 112 city
modify_profile_delete_city(Config) ->
    User = ?config(user, Config),
    ok = em_event_server:process_event(city112(User, 'Nuuk')),
    ok = em_event_server:process_event(nil_city112(User)).
    

% Promote user to trunk user (user has default pubid)
modify_user_default_pubid_to_trunkuser(Config) ->
    User = ?config(user, Config),
    Phone1 = ?config(phone1, Config),
    ok = em_event_server:process_event(set_trunkuser(User, Phone1, User)).

% Promote user to trunk user (user has non-default pubid)
modify_user_pubid_to_trunkuser(Config) ->
    User = ?config(user, Config),
    Phone1 = ?config(phone1, Config),
    PubId1 = ?config(pubid1, Config),
    ok = em_event_server:process_event(set_trunkuser(User, Phone1, PubId1)).


% modify trunkuser to non-default pubid
modify_trunkuser_pubid(Config) ->
    User = ?config(user, Config),
    Phone1 = ?config(phone1, Config),
    PubId1 = ?config(pubid1, Config),
    ok = em_event_server:process_event(set_trunkuser(User, Phone1, User)),
    ok = em_event_server:process_event(set_trunkuser(User, Phone1, PubId1)).

modify_trunkuser_phone(Config) ->
    User = ?config(user, Config),
    Phone1 = ?config(phone1, Config),
    Phone2 = ?config(phone2, Config),
    ok = em_event_server:process_event(set_trunkuser(User, Phone1, User)),
    ok = em_event_server:process_event(set_trunkuser(User, Phone2, User)).

modify_trunkuser_to_user(Config) ->
    User = ?config(user, Config),
    Phone1 = ?config(phone1, Config),
    ok = em_event_server:process_event(set_trunkuser(User, Phone1, User)),
    ok = em_event_server:process_event(trunkuser_to_user(User, Phone1, User)).

modify_nonexisting_user(Config) ->
    User = 'moduser011@lab24.timezone4.com',
    Phone1 = ?config(phone1, Config),
    error = em_event_server:process_event(set_set(User, Phone1, User)).    
    
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

nil_nil(User) ->
    io_lib:format("<BroadsoftOCIReportingDocument protocol=\"OCIReporting\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\">
	<command xsi:type=\"OCIReportingReportNotification\">
		<id>write266</id>
		<userId>admin</userId>
		<loginType>System</loginType>
		<request><![CDATA[<?xml version=\"1.0\" encoding=\"ISO-8859-1\"?>
			<BroadsoftDocument protocol=\"OCI\" xmlns=\"C\">
				<userId xmlns=\"\">admin</userId>
				<command xsi:type=\"UserModifyRequest17sp4\" xmlns=\"\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\">
					<userId>~s</userId>
					<phoneNumber xsi:nil=\"true\"/>
					<extension>7777</extension>
					<sipAliasList xsi:nil=\"true\"/>
					<endpoint xsi:nil=\"true\"/>
				</command>
			</BroadsoftDocument>]]>
		</request>
	</command>
</BroadsoftOCIReportingDocument>",[User]).    

set_nil(User, Phone) ->
    io_lib:format("<BroadsoftOCIReportingDocument protocol=\"OCIReporting\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\">
	<command xsi:type=\"OCIReportingReportNotification\">
		<id>write266</id>
		<userId>admin</userId>
		<loginType>System</loginType>
		<request><![CDATA[<?xml version=\"1.0\" encoding=\"ISO-8859-1\"?>
			<BroadsoftDocument protocol=\"OCI\" xmlns=\"C\">
				<userId xmlns=\"\">admin</userId>
				<command xsi:type=\"UserModifyRequest17sp4\" xmlns=\"\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\">
					<userId>~s</userId>
					<phoneNumber>~s</phoneNumber>
					<extension>7777</extension>
					<sipAliasList xsi:nil=\"true\"/>
					<endpoint xsi:nil=\"true\"/>
				</command>
			</BroadsoftDocument>]]>
		</request>
	</command>
</BroadsoftOCIReportingDocument>",[User, Phone]).

nil_set(User, PubId) ->
    io_lib:format("<BroadsoftOCIReportingDocument protocol=\"OCIReporting\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\">
	<command xsi:type=\"OCIReportingReportNotification\">
		<id>write267</id>
		<userId>admin</userId>
		<loginType>System</loginType>
		<request><![CDATA[<?xml version=\"1.0\" encoding=\"ISO-8859-1\"?>
			<BroadsoftDocument protocol=\"OCI\" xmlns=\"C\">
				<userId xmlns=\"\">admin</userId>
				<command xsi:type=\"UserModifyRequest17sp4\" xmlns=\"\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\">
					<userId>~s</userId>
					<phoneNumber xsi:nil=\"true\"/>
					<extension>7777</extension>
					<sipAliasList xsi:nil=\"true\"/>
					<endpoint>
						<accessDeviceEndpoint>
							<accessDevice>
								<deviceLevel>Group</deviceLevel>
								<deviceName>Test</deviceName>
							</accessDevice>
							<linePort>~s</linePort>
							<contactList xsi:nil=\"true\"/>
						</accessDeviceEndpoint>
					</endpoint>
				</command>
			</BroadsoftDocument>]]>
		</request>
	</command>
</BroadsoftOCIReportingDocument>",[User, PubId]).


set_set(User, Phone, PubId) ->
    io_lib:format("<BroadsoftOCIReportingDocument protocol=\"OCIReporting\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\">
	<command xsi:type=\"OCIReportingReportNotification\">
		<id>write267</id>
		<userId>admin</userId>
		<loginType>System</loginType>
		<request><![CDATA[<?xml version=\"1.0\" encoding=\"ISO-8859-1\"?>
			<BroadsoftDocument protocol=\"OCI\" xmlns=\"C\">
				<userId xmlns=\"\">admin</userId>
				<command xsi:type=\"UserModifyRequest17sp4\" xmlns=\"\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\">
					<userId>~s</userId>
					<phoneNumber>~s</phoneNumber>
					<extension>7777</extension>
					<sipAliasList xsi:nil=\"true\"/>
					<endpoint>
						<accessDeviceEndpoint>
							<accessDevice>
								<deviceLevel>Group</deviceLevel>
								<deviceName>Test</deviceName>
							</accessDevice>
							<linePort>~s</linePort>
							<contactList xsi:nil=\"true\"/>
						</accessDeviceEndpoint>
					</endpoint>
				</command>
			</BroadsoftDocument>]]>
		</request>
	</command>
</BroadsoftOCIReportingDocument>",[User, Phone, PubId]).


city112(User, City) ->
    io_lib:format("<BroadsoftOCIReportingDocument protocol=\"OCIReporting\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\">
    <command xsi:type=\"OCIReportingReportNotification\">
        <id>write272</id>
        <userId>admin</userId>
        <loginType>System</loginType>
        <request><![CDATA[<?xml version=\"1.0\" encoding=\"ISO-8859-1\"?>
            <BroadsoftDocument protocol=\"OCI\" xmlns=\"C\">
                <userId xmlns=\"\">admin</userId>
                <command xsi:type=\"UserModifyRequest17sp4\" xmlns=\"\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\">
                    <userId>~s</userId>
                    <lastName>user005</lastName>
                    <firstName>user005</firstName>
                    <callingLineIdLastName>user005</callingLineIdLastName>
                    <callingLineIdFirstName>user005</callingLineIdFirstName>
                    <nameDialingName xsi:nil=\"true\"/>
                    <department xsi:type=\"GroupDepartmentKey\" xsi:nil=\"true\"/>
                    <language>English</language>
                    <timeZone>Europe/Copenhagen</timeZone>
                    <title xsi:nil=\"true\"/>
                    <pagerPhoneNumber xsi:nil=\"true\"/>
                    <mobilePhoneNumber xsi:nil=\"true\"/>
                    <emailAddress xsi:nil=\"true\"/>
                    <yahooId xsi:nil=\"true\"/>
                    <addressLocation xsi:nil=\"true\"/>
                    <address>
                        <addressLine1 xsi:nil=\"true\"/>
                        <addressLine2 xsi:nil=\"true\"/>
                        <city xsi:nil=\"true\"/>
                        <stateOrProvince>~s</stateOrProvince>
                        <zipOrPostalCode xsi:nil=\"true\"/>
                        <country xsi:nil=\"true\"/>
                    </address>
                </command>
            </BroadsoftDocument>]]>
        </request>
    </command>
</BroadsoftOCIReportingDocument>",[User, City]).

nil_city112(User) ->
    io_lib:format("<BroadsoftOCIReportingDocument protocol=\"OCIReporting\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\">
    <command xsi:type=\"OCIReportingReportNotification\">
        <id>write272</id>
        <userId>admin</userId>
        <loginType>System</loginType>
        <request><![CDATA[<?xml version=\"1.0\" encoding=\"ISO-8859-1\"?>
            <BroadsoftDocument protocol=\"OCI\" xmlns=\"C\">
                <userId xmlns=\"\">admin</userId>
                <command xsi:type=\"UserModifyRequest17sp4\" xmlns=\"\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\">
                    <userId>~s</userId>
                    <lastName>user005</lastName>
                    <firstName>user005</firstName>
                    <callingLineIdLastName>user005</callingLineIdLastName>
                    <callingLineIdFirstName>user005</callingLineIdFirstName>
                    <nameDialingName xsi:nil=\"true\"/>
                    <department xsi:type=\"GroupDepartmentKey\" xsi:nil=\"true\"/>
                    <language>English</language>
                    <timeZone>Europe/Copenhagen</timeZone>
                    <title xsi:nil=\"true\"/>
                    <pagerPhoneNumber xsi:nil=\"true\"/>
                    <mobilePhoneNumber xsi:nil=\"true\"/>
                    <emailAddress xsi:nil=\"true\"/>
                    <yahooId xsi:nil=\"true\"/>
                    <addressLocation xsi:nil=\"true\"/>
                    <address>
                        <addressLine1 xsi:nil=\"true\"/>
                        <addressLine2 xsi:nil=\"true\"/>
                        <city xsi:nil=\"true\"/>
                        <stateOrProvince xsi:nil=\"true\"/>
                        <zipOrPostalCode xsi:nil=\"true\"/>
                        <country xsi:nil=\"true\"/>
                    </address>
                </command>
            </BroadsoftDocument>]]>
        </request>
    </command>
</BroadsoftOCIReportingDocument>",[User]).



set_trunkuser(User, Phone, PubId) ->
    io_lib:format("<BroadsoftOCIReportingDocument protocol=\"OCIReporting\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\">
    <command xsi:type=\"OCIReportingReportNotification\">
        <id>write275</id>
        <userId>admin</userId>
        <loginType>System</loginType>
        <request><![CDATA[<?xml version=\"1.0\" encoding=\"ISO-8859-1\"?>
            <BroadsoftDocument protocol=\"OCI\" xmlns=\"C\">
                <userId xmlns=\"\">admin</userId>
                <command xsi:type=\"UserModifyRequest17sp4\" xmlns=\"\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\">
                    <userId>~s</userId>
                    <phoneNumber>~s</phoneNumber>
                    <extension>9999</extension>
                    <sipAliasList xsi:nil=\"true\"/>
                    <endpoint>
                        <trunkAddressing>
                            <trunkGroupDeviceEndpoint>
                                <name>tggrp1</name>
                                <linePort>~s</linePort>
                                <contactList xsi:nil=\"true\"/>
                            </trunkGroupDeviceEndpoint>
                            <enterpriseTrunkName xsi:nil=\"true\"/>
                            <alternateTrunkIdentity xsi:nil=\"true\"/>
                            <alternateTrunkIdentityDomain xsi:nil=\"true\"/>
                        </trunkAddressing>
                    </endpoint>
                </command>
            </BroadsoftDocument>]]>
        </request>
    </command>
</BroadsoftOCIReportingDocument>",[User, Phone, PubId]).    


trunkuser_to_user(User, Phone, PubId) ->
    io_lib:format("<BroadsoftOCIReportingDocument protocol=\"OCIReporting\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\">
    <command xsi:type=\"OCIReportingReportNotification\">
        <id>write277</id>
        <userId>admin</userId>
        <loginType>System</loginType>
        <request><![CDATA[<?xml version=\"1.0\" encoding=\"ISO-8859-1\"?>
            <BroadsoftDocument protocol=\"OCI\" xmlns=\"C\">
                <userId xmlns=\"\">admin</userId>
                <command xsi:type=\"UserModifyRequest17sp4\" xmlns=\"\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\">
                    <userId>~s</userId>
                    <phoneNumber>~s</phoneNumber>
                    <extension>9999</extension>
                    <sipAliasList xsi:nil=\"true\"/>
                    <endpoint>
                        <accessDeviceEndpoint>
                            <accessDevice>
                                <deviceLevel>Group</deviceLevel>
                                <deviceName>user005</deviceName>
                            </accessDevice>
                            <linePort>~s</linePort>
                            <contactList xsi:nil=\"true\"/>
                        </accessDeviceEndpoint>
                    </endpoint>
                </command>
            </BroadsoftDocument>]]>
        </request>
    </command>
</BroadsoftOCIReportingDocument>",[User, Phone, PubId]).    

    