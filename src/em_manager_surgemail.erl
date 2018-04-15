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

-module(em_manager_surgemail).
-include("../include/em.hrl").

%% API
-export([create_domain/1]).
-export([delete_domain/1]).
-export([create_account/2]).
-export([delete_account/1]).
-export([modify/1]).


%%%===================================================================
%%% API
%%%===================================================================
create_domain(Domain) ->
    {ok, Hosts} = application:get_env(em, em_surgemail),
    Primary = proplists:get_value(primary, Hosts),
    Username = proplists:get_value(username, Primary),
    Password = proplists:get_value(password, Primary),

    Param1 = "/cgi/admin.cgi?show=simple_msg.xml&",
    Param2 = "cmd=global_misc_save&",
    Param3 = "misc_settings=domain_name,manager_username,manager_password,create_user,create_max&",
    Param4 = "misc_cmd=special&",
    Param5 = "domainid=-1&",
    Param6 = "name=" ++ Domain ++ "&",
    Param7 = "manager_username=" ++ Username ++ "&",
    Param8 = "manager_password=" ++ Password ++ "" ,
    
    Request = Param1 ++ Param2 ++ Param3 ++ Param4 ++ Param5 ++ Param6 ++ Param7 ++ Param8,     
    em_surgemail:request(Request).

delete_domain(Domain) ->
    Param1 = "/cgi/admin.cgi?show=simple_msg.xml&",
    Param2 = "cmd=domain_delete&",
    Param3 = "domain=" ++ Domain ++ "&",
    Param4 = "delete_users=true&",
    Param5 = "delete_files=true",
    
    Request = Param1 ++ Param2 ++ Param3 ++ Param4 ++ Param5,     
    em_surgemail:request(Request).

create_account(MailUser, MailPass) ->
    {ok, Args} = application:get_env(em, em_surgemail),
    DomainPass = proplists:get_value(domain_password, Args),
    [UserPart, DomainPart] = string:split(MailUser, "@"), 
    
    Param1 = "/cgi/domadmin.cgi?show=simple_msg.xml&",
    Param2 = "cmd=cmd_user_login&",
    Param3 = "lcmd=user_create&",
    Param4 = "user_fields=user_id&",
    Param5 = "username=" ++ "admin@" ++ DomainPart ++ "&",
    Param6 = "password=" ++ DomainPass ++ "&",
    Param7 = "lusername=" ++ UserPart ++ "&",
    Param8 = "lpassword=" ++ MailPass ++ "",

    Request = Param1 ++ Param2 ++ Param3 ++ Param4 ++ Param5 ++ Param6 ++ Param7 ++ Param8, 
    em_surgemail:request(Request).

delete_account(MailUser) ->
    {ok, Args} = application:get_env(em, em_surgemail),
    DomainPass = proplists:get_value(domain_password, Args),
    [UserPart, DomainPart] = string:split(MailUser, "@"), 
    
    Param1 = "/cgi/domadmin.cgi?show=simple_msg.xml&",
    Param2 = "cmd=cmd_user_login&",
    Param3 = "lcmd=user_delete&",
    Param4 = "user_fields=user_id&",
    Param5 = "username=" ++ "admin@" ++ DomainPart ++ "&",
    Param6 = "password=" ++ DomainPass ++ "&",
    Param7 = "lusername=" ++ UserPart ++ "&",

    Request = Param1 ++ Param2 ++ Param3 ++ Param4 ++ Param5 ++ Param6 ++ Param7, 
    em_surgemail:request(Request).
    
modify(Event) ->
    User = maps:get(user, Event),
    MailUser = maps:get(mailuser, Event),
    MailPass = maps:get(mailpass, Event),
    CurrentMailUser = maps:get(current_mailuser, Event),
    
    case {MailUser, MailPass, CurrentMailUser} of
        {undefined, undefined, _} -> 
            ?INFO_MSG("Ignoring (mailuser do not exist): ~n", []), 
            ok;
            
        {X, _, X} -> 
            ?INFO_MSG("Ignoring (mailuser already set): ~n", []), 
            ok;
            
        {_, _, undefined} -> 
            ?INFO_MSG("creating vmail accpount: ~p~n", [MailUser]),
            em_srd:set_vmail(User, MailUser, MailPass),
            create_account(MailUser, MailPass)
    end.
    
%%%===================================================================
%%% Internal functions
%%%===================================================================
   
   
%%%===================================================================
%%% Unit Tests
%%%===================================================================
%-ifdef(TEST).
%-include_lib("eunit/include/eunit.hrl").

%plan_pubid_change_test_() ->
%    [?_assert(plan_pubid_change(nil, "user@tz4.com", "user@tz4.com") =:= ignore),
%     ?_assert(plan_pubid_change(nil, "hello@tz4.com", "user@tz4.com") =:= delete),
%     ?_assert(plan_pubid_change("hello@tz4.com", "hello@tz4.com", "user@tz4.com") =:= ignore),
%     ?_assert(plan_pubid_change("hello@tz4.com", "world@tz4.com", "user@tz4.com") =:= update)
%    ].
    
% plan_phone_change_test_() ->
%    [?_assert(plan_phone_change(nil, "NODATA") =:= ignore),
%     ?_assert(plan_phone_change(nil, "299123456") =:= delete),
%     ?_assert(plan_phone_change(undefined, "299123456") =:= delete),     
%     ?_assert(plan_phone_change("299123456", "299123456") =:= ignore),
%     ?_assert(plan_phone_change("299123456", "NODATA") =:= create),
%     ?_assert(plan_phone_change("299123456", "111111") =:= update)
%    ].
%-endif.       
   
