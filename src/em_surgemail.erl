%%% Copyright 2017 <thomas.elsgaard@timezone4.com>
%%%
%%% Licensed under the Apache License, Version 2.0 (the "License");
%%% you may not use this file except in compliance with the License.
%%% You may obtain a copy of the License at
%%%
%%%   http://www.apache.org/licenses/LICENSE-2.0
%%%
%%% Unless required by applicable law or agreed to in writing, software
%%% distributed under the License is distributed on an "AS IS" BASIS,
%%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%% See the License for the specific language governing permissions and
%%% limitations under the License.

-module(em_surgemail).
-behaviour(gen_server).
-include("../include/em.hrl").

%% API
-export([start_link/0]).
-export([create_domain/1]).
-export([delete_domain/1]).
-export([create_account/2]).
-export([delete_account/1]).
-export([modify/1]).




%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {surgemail_hosts}).

%%%===================================================================
%%% API
%%%===================================================================

create_domain(Domain) ->
    gen_server:call(?SERVER, {create_domain, Domain}).

delete_domain(Domain) ->
    gen_server:call(?SERVER, {delete_domain, Domain}, 10000).

create_account(MailUser, MailPass) ->
    gen_server:call(?SERVER, {create_account, MailUser, MailPass}).

delete_account(MailUser) ->
    gen_server:call(?SERVER, {delete_account, MailUser}, 10000).
    
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
            gen_server:call(?SERVER, {create_account, MailUser, MailPass})
    end.
    
    
%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    process_flag(trap_exit, true),
    {ok, Args} = application:get_env(em, em_surgemail),
    {ok, #state{surgemail_hosts = Args}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call({create_domain, Domain}, _From, State=#state{surgemail_hosts = Hosts}) ->
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
    
    {ok, _} = send(Request, State),
    {reply, ok, State};

handle_call({delete_domain, Domain}, _From, State) ->
    Param1 = "/cgi/admin.cgi?show=simple_msg.xml&",
    Param2 = "cmd=domain_delete&",
    Param3 = "domain=" ++ Domain ++ "&",
    Param4 = "delete_users=true&",
    Param5 = "delete_files=true",
    
    Request = Param1 ++ Param2 ++ Param3 ++ Param4 ++ Param5, 
    
    {ok, _} = send(Request, State),
    {reply, ok, State};


handle_call({create_account, MailUser, MailPass}, _From, State=#state{surgemail_hosts = Hosts}) ->
    DomainPass = proplists:get_value(domain_password, Hosts),
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
    
    {ok, _} = send(Request, State),
    {reply, ok, State};


handle_call({delete_account, MailUser}, _From, State=#state{surgemail_hosts = Hosts}) ->
    DomainPass = proplists:get_value(domain_password, Hosts),
    [UserPart, DomainPart] = string:split(MailUser, "@"), 
    
    Param1 = "/cgi/domadmin.cgi?show=simple_msg.xml&",
    Param2 = "cmd=cmd_user_login&",
    Param3 = "lcmd=user_delete&",
    Param4 = "user_fields=user_id&",
    Param5 = "username=" ++ "admin@" ++ DomainPart ++ "&",
    Param6 = "password=" ++ DomainPass ++ "&",
    Param7 = "lusername=" ++ UserPart ++ "&",

    Request = Param1 ++ Param2 ++ Param3 ++ Param4 ++ Param5 ++ Param6 ++ Param7, 
    
    {ok, _} = send(Request, State),
    {reply, ok, State};

        
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
  
send(Request, State=#state{surgemail_hosts = Hosts}) ->
    ?INFO_MSG("Trying to send request to primary Surgemail: ~n", []), 
    Primary = proplists:get_value(primary, Hosts),
    Hostname = proplists:get_value(hostname, Primary),
    Port = proplists:get_value(port, Primary),
    Username = proplists:get_value(username, Primary),
    Password = proplists:get_value(password, Primary),
    Url = "http://" ++ Hostname ++ ":" ++ Port ++ Request, 
    %?INFO_MSG("Surgemail URL: ~p~n", [Url]), 

    ContentType = "text/html",
    Headers = [auth_header(Username, Password), {"Content-Type", ContentType}],
    case httpc:request(post, {Url, Headers, ContentType, ""}, [{timeout,4000}], []) of
            {ok,{{_,200,_}, _Headers, Body}}-> {ok, Body};
            {ok,{{_,_OtherStatus,_},_Headers,_Body}} ->
                exit(http_code_unexpected);
            {error, {failed_connect, _Error}} ->
                ?INFO_MSG("Connect failed towards primary surgemail: ~p~n", [Hostname]),
                send_secondary(Request, State);
            {error, Reason} ->
                exit(Reason)
    end. 

send_secondary(Request, #state{surgemail_hosts = Hosts}) ->
    ?INFO_MSG("Trying to send request to secondary Surgemail: ~n", []), 
    Secondary = proplists:get_value(secondary, Hosts),
    Hostname = proplists:get_value(hostname, Secondary),
    Port = proplists:get_value(port, Secondary),
    Username = proplists:get_value(username, Secondary),
    Password = proplists:get_value(password, Secondary),
    Url = "http://" ++ Hostname ++ ":" ++ Port ++ Request, 
    %?INFO_MSG("Surgemail URL: ~p~n", [Url]), 

    ContentType = "text/html",
    Headers = [auth_header(Username, Password), {"Content-Type", ContentType}],
    case httpc:request(post, {Url, Headers, ContentType, ""}, [{timeout,4000}], []) of
            {ok,{{_,200,_}, _Headers, Body}}-> {ok, Body};
            {ok,{{_,_OtherStatus,_},_Headers,_Body}} ->
                exit(http_code_unexpected);
            {error, {failed_connect, Error}} ->
                ?INFO_MSG("Connect failed towards secondary surgemail: ~p~n", [Hostname]),
                {error, Error};
            {error, Reason} ->
                exit(Reason)
    end. 
        
    
auth_header(Username, Password) ->
    Encoded = base64:encode_to_string(lists:append([Username,":",Password])),
    {"Authorization","Basic " ++ Encoded}.
 


