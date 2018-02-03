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

-module(em_processor_vmail).

-export([create_domain/1, delete_domain/1, modify/1, delete_account/1]).
-include("../include/em.hrl").

%%%===================================================================
%%% API
%%%===================================================================

create_domain(Domain) ->
    Host = "http://10.8.10.8:7026/cgi/admin.cgi?",
    Param1 = "show=simple_msg.xml&",
    Param2 = "cmd=global_misc_save&",
    Param3 = "misc_settings=domain_name,manager_username,manager_password,create_user,create_max&",
    Param4 = "misc_cmd=special&",
    Param5 = "domainid=-1&",
    Param6 = "name=" ++ Domain ++ "&",
    Param7 = "manager_username=admin&",
    Param8 = "manager_password=ics5ics5",
    
    Url = Host ++ Param1 ++ Param2 ++ Param3 ++ Param4 ++ Param5 ++ Param6 ++ Param7 ++ Param8, 
    do_request(Url).
    %ibrowse:send_req(Url, [], post, [], [{basic_auth, {"admin", "ics5ics5"}}]).
    
delete_domain(Domain) ->
    Host = "http://10.8.10.8:7026/cgi/admin.cgi?",
    Param1 = "show=simple_msg.xml&",
    Param2 = "cmd=domain_delete&",
    Param3 = "domain=" ++ Domain ++ "&",
    Param4 = "delete_users=true&",
    Param5 = "delete_files=true",
    
    Url = Host ++ Param1 ++ Param2 ++ Param3 ++ Param4 ++ Param5, 
    do_request(Url).
    %ibrowse:send_req(Url, [], post, [], [{basic_auth, {"admin", "ics5ics5"}}]).


modify(#vmevent{mailuser=undefined, mailpass=undefined}) ->
    ?INFO_MSG("Ignoring (all undefined): ~n", []); 

modify(#vmevent{mailuser=X1, current_mailuser=X1}) ->
    ?INFO_MSG("Ignoring (mailuser already set): ~n", []);
    
modify(#vmevent{user=UserName, mailuser=X1, current_mailuser=undefined, mailpass=X3}) ->
    ?INFO_MSG("creating vmail accpount (mailuser not existing): ~n", []),
    create_account(X1, X3),
    em_srd:set_vmail(UserName, X1, X3);

modify(#vmevent{mailuser=X1, current_mailuser=X2, mailpass=X3, current_mailpass=X4}) ->
    ?INFO_MSG("mailuser: ~p", [X1]),
    ?INFO_MSG("current_mailuser: ~p", [X2]),
    ?INFO_MSG("mailpass: ~p", [X3]),
    ?INFO_MSG("current_mailuser: ~p", [X4]).
    
create_account(MailUser, MailPass) ->
    DomainPass = "ics5ics5",
    
    [UserPart, DomainPart] = string:split(MailUser, "@"), 
    
    Host = "http://10.8.10.8:7026/cgi/domadmin.cgi?",
    Param1 = "show=simple_msg.xml&",
    Param2 = "cmd=cmd_user_login&",
    Param3 = "lcmd=user_create&",
    Param4 = "user_fields=user_id&",
    Param5 = "username=" ++ "admin@" ++ DomainPart ++ "&",
    Param6 = "password=" ++ DomainPass ++ "&",
    Param7 = "lusername=" ++ UserPart ++ "&",
    Param8 = "lpassword=" ++ MailPass ++ "",
    
    Url = Host ++ Param1 ++ Param2 ++ Param3 ++ Param4 ++ Param5 ++ Param6 ++ Param7 ++ Param8, 
    do_request(Url).
    %ibrowse:send_req(Url, [], post).


delete_account(MailUser) ->
    DomainPass = "ics5ics5",
    
    [UserPart, DomainPart] = string:split(MailUser, "@"),
    
    Host = "http://10.8.10.8:7026/cgi/domadmin.cgi?",
    Param1 = "show=simple_msg.xml&",
    Param2 = "cmd=cmd_user_login&",
    Param3 = "lcmd=user_delete&",
    Param4 = "user_fields=user_id&",
    Param5 = "username=" ++ "admin@" ++ DomainPart ++ "&",
    Param6 = "password=" ++ DomainPass ++ "&",
    Param7 = "lusername=" ++ UserPart ++ "",
    
    Url = Host ++ Param1 ++ Param2 ++ Param3 ++ Param4 ++ Param5 ++ Param6 ++ Param7, 
    do_request(Url).
    %ibrowse:send_req(Url, [], post).


do_request(Url) ->
    ContentType = "text/html",
    Headers = [auth_header(?VMAIL_USER, ?VMAIL_PASS), {"Content-Type",ContentType}],
    httpc:request(post, {Url, Headers, ContentType, ""}, [], []).
    
auth_header(User, Pass) ->
    Encoded = base64:encode_to_string(lists:append([User,":",Pass])),
    {"Authorization","Basic " ++ Encoded}.
    
    