%% Copyright 2017 <thomas.elsgaard@middleaware.dk>
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

-module(em_surgemail).
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
    Username = econfig:get_value(em, "surgemail", "username"),
    Password = econfig:get_value(em, "surgemail", "password"),

    Param1 = "/cgi/admin.cgi?show=simple_msg.xml&",
    Param2 = "cmd=global_misc_save&",
    Param3 = "misc_settings=domain_name,manager_username,manager_password,create_user,create_max&",
    Param4 = "misc_cmd=special&",
    Param5 = "domainid=-1&",
    Param6 = "name=" ++ Domain ++ "&",
    Param7 = "manager_username=" ++ Username ++ "&",
    Param8 = "manager_password=" ++ Password ++ "" ,
    
    Request = Param1 ++ Param2 ++ Param3 ++ Param4 ++ Param5 ++ Param6 ++ Param7 ++ Param8,     
    send(Request).

delete_domain(Domain) ->
    Param1 = "/cgi/admin.cgi?show=simple_msg.xml&",
    Param2 = "cmd=domain_delete&",
    Param3 = "domain=" ++ Domain ++ "&",
    Param4 = "delete_users=true&",
    Param5 = "delete_files=true",
    
    Request = Param1 ++ Param2 ++ Param3 ++ Param4 ++ Param5,     
    send(Request).

create_account(MailUser, MailPass) ->
    DomainPass = econfig:get_value(em, "surgemail", "domain_password"),
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
    send(Request).

delete_account(#{ user := User }) ->
    case em_srd:get_vmail_user(User) of
        undefined -> ok;
        "NODATA" -> ok;
        MailUser -> do_delete_account(User, MailUser)
    end.
    
    
modify(Event) ->
    User = maps:get(user, Event),
    MailUser = maps:get(mailuser, Event),
    MailPass = maps:get(mailpass, Event),
    CurrentMailUser = maps:get(current_mailuser, Event),
    
    case {MailUser, MailPass, CurrentMailUser} of
        {undefined, undefined, _} -> 
            logger:debug("Ignoring (mailuser do not exist)"), 
            ok;
            
        {X, _, X} -> 
            logger:debug("Ignoring (mailuser already set)"), 
            ok;
            
        {_, _, undefined} -> 
            logger:debug("creating vmail accpount: ~s", [MailUser]),
            em_srd:set_vmail(User, MailUser, MailPass),
            create_account(MailUser, MailPass)
    end.
    
%%%===================================================================
%%% Internal functions
%%%===================================================================
do_delete_account(User, MailUser) ->
    logger:notice("Deleting surgemail account: ~s", [MailUser]), 
    DomainPass = econfig:get_value(em, "surgemail", "domain_password"),
    [UserPart, DomainPart] = string:split(MailUser, "@"), 
    
    Param1 = "/cgi/domadmin.cgi?show=simple_msg.xml&",
    Param2 = "cmd=cmd_user_login&",
    Param3 = "lcmd=user_delete&",
    Param4 = "user_fields=user_id&",
    Param5 = "username=" ++ "admin@" ++ DomainPart ++ "&",
    Param6 = "password=" ++ DomainPass ++ "&",
    Param7 = "lusername=" ++ UserPart ++ "&",

    Request = Param1 ++ Param2 ++ Param3 ++ Param4 ++ Param5 ++ Param6 ++ Param7,
    send(Request),
    em_srd:delete_vmail(User).
     
send(Request) ->
    logger:debug("Sending request to primary Surgemail"), 
    Hostname = econfig:get_value(em, "surgemail", "primary_host"),
    Port = econfig:get_value(em, "surgemail", "port"),
    Username = econfig:get_value(em, "surgemail", "username"),
    Password = econfig:get_value(em, "surgemail", "password"),
    
    Url = "http://" ++ Hostname ++ ":" ++ Port ++ Request, 

    ContentType = "text/html",
    Headers = [auth_header(Username, Password), {"Content-Type", ContentType}],
    case httpc:request(post, {Url, Headers, ContentType, ""}, [{timeout,4000}], []) of
            {ok,{{_,200,_}, _Headers, Body}}-> {ok, Body};
            {ok,{{_,_OtherStatus,_},_Headers,_Body}} ->
                exit(http_code_unexpected);
            {error, {failed_connect, _Error}} ->
                logger:error("Connect failed towards primary surgemail: ~s", [Hostname]),
                send_secondary(Request);
            {error, Reason} ->
                exit(Reason)
    end. 

send_secondary(Request) ->
    logger:debug("Sending request to secondary Surgemail"), 
    Hostname = econfig:get_value(em, "surgemail", "secondary_host"),
    Port = econfig:get_value(em, "surgemail", "port"),
    Username = econfig:get_value(em, "surgemail", "username"),
    Password = econfig:get_value(em, "surgemail", "password"),
    Url = "http://" ++ Hostname ++ ":" ++ Port ++ Request, 
    %?INFO_MSG("Surgemail URL: ~p~n", [Url]), 

    ContentType = "text/html",
    Headers = [auth_header(Username, Password), {"Content-Type", ContentType}],
    case httpc:request(post, {Url, Headers, ContentType, ""}, [{timeout,4000}], []) of
            {ok,{{_,200,_}, _Headers, Body}}-> {ok, Body};
            {ok,{{_,_OtherStatus,_},_Headers,_Body}} ->
                exit(http_code_unexpected);
            {error, {failed_connect, Error}} ->
                logger:error("Connect failed towards secondary surgemail: ~s", [Hostname]),
                {error, Error};
            {error, Reason} ->
                exit(Reason)
    end. 
        
    
auth_header(Username, Password) ->
    Encoded = base64:encode_to_string(lists:append([Username,":",Password])),
    {"Authorization","Basic " ++ Encoded}.

   
%%%===================================================================
%%% Unit Tests
%%%===================================================================
%-ifdef(TEST).
%-include_lib("eunit/include/eunit.hrl").
%-endif.       
   
