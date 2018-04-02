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

-module(em_ema_server).
-behaviour(gen_server).

%% API
-export([start_link/0]).
-export([create_hss_subscriber/1]).
-export([create_hss_virtual_subscriber/1]).
-export([create_hss_serviceprofile/1]).
-export([create_hss_pubid/1]).
-export([create_hss_teluri/1]).
-export([delete_hss_subscriber/1]).
-export([delete_hss_serviceprofile/2]).
-export([delete_hss_pubid/2]).
-export([delete_hss_teluri/2]).
-export([update_hss_pass/1]).
-export([update_hss_phonecontext/1]).
-export([create_enum/1]).
-export([delete_enum/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {ema_host, ema_user, ema_pass}).

%%%===================================================================
%%% API
%%%===================================================================

create_hss_subscriber(IMSAssociation) ->
	gen_server:call(?SERVER, {create_hss_subscriber, IMSAssociation}).

create_hss_virtual_subscriber(IMSAssociation) ->
	gen_server:call(?SERVER, {create_hss_virtual_subscriber, IMSAssociation}).

create_hss_serviceprofile(IMSAssociation) ->
	gen_server:call(?SERVER, {create_hss_serviceprofile, IMSAssociation}).

create_hss_pubid(IMSAssociation) ->
	gen_server:call(?SERVER, {create_hss_pubid, IMSAssociation}).

create_hss_teluri(IMSAssociation) ->
	gen_server:call(?SERVER, {create_hss_teluri, IMSAssociation}).

delete_hss_subscriber(IMSAssociation) ->
	gen_server:call(?SERVER, {delete_hss_subscriber, IMSAssociation}).

delete_hss_serviceprofile(User, ServiceProfile) ->
	gen_server:call(?SERVER, {delete_hss_serviceprofile, User, ServiceProfile}).

delete_hss_pubid(User, PubId) ->
	gen_server:call(?SERVER, {delete_hss_pubid, User, PubId}).

delete_hss_teluri(User, Phone) ->
	gen_server:call(?SERVER, {delete_hss_teluri, User, Phone}).

update_hss_pass(IMSAssociation) ->
	gen_server:call(?SERVER, {update_hss_pass, IMSAssociation}).

update_hss_phonecontext(IMSAssociation) ->
	gen_server:call(?SERVER, {update_hss_phonecontext, IMSAssociation}).

create_enum(IMSAssociation) ->
	gen_server:call(?SERVER, {create_enum, IMSAssociation}).

delete_enum(Phone) ->
	gen_server:call(?SERVER, {delete_enum, Phone}).

    
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
    {ok,{Host, User, Pass}} = application:get_env(em, ema),
    {ok, #state{ema_host = Host, ema_user = User, ema_pass = Pass}}.

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
handle_call({create_hss_subscriber, IMSAssociation}, _From, State) ->
    Session = open_session(State),
    Request = em_interface_cai3g_envelopes:add_ims_subscriber(Session, IMSAssociation),
    {ok, _} = send(Request, State),
    {reply, ok, State};
    
handle_call({create_hss_virtual_subscriber, IMSAssociation}, _From, State) ->
    Session = open_session(State),
    Request = em_interface_cai3g_envelopes:add_ims_virtual_subscriber(Session, IMSAssociation),
    {ok, _} = send(Request, State),
    close_session(Session, State),
    {reply, ok, State};

handle_call({create_hss_serviceprofile, IMSAssociation}, _From, State) ->
    Session = open_session(State),
    Request = em_interface_cai3g_envelopes:add_ims_serviceprofile(Session, IMSAssociation),
    {ok, _} = send(Request, State),
    close_session(Session, State),
    {reply, ok, State};

handle_call({create_hss_pubid, IMSAssociation}, _From, State) ->
    Session = open_session(State),
    Request = em_interface_cai3g_envelopes:add_ims_pubid(Session, IMSAssociation),
    {ok, _} = send(Request, State),
    close_session(Session, State),
    {reply, ok, State};

handle_call({create_hss_teluri, IMSAssociation}, _From, State) ->
    Session = open_session(State),
    Request = em_interface_cai3g_envelopes:add_ims_teluri(Session, IMSAssociation),
    {ok, _} = send(Request, State),
    close_session(Session, State),
    {reply, ok, State};

handle_call({delete_hss_subscriber, IMSAssociation}, _From, State) ->
    Session = open_session(State),
    Request = em_interface_cai3g_envelopes:delete_ims_subscriber(Session, IMSAssociation),
    {ok, _} = send(Request, State),
    close_session(Session, State),
    {reply, ok, State};

handle_call({delete_hss_serviceprofile, User, ServiceProfile}, _From, State) ->
    Session = open_session(State),
    Request = em_interface_cai3g_envelopes:delete_ims_serviceprofile(Session, User, ServiceProfile),
    {ok, _} = send(Request, State),
    close_session(Session, State),
    {reply, ok, State};

handle_call({delete_hss_pubid, User, PubId}, _From, State) ->
    Session = open_session(State),
    Request = em_interface_cai3g_envelopes:delete_ims_pubid(Session, User, PubId),
    {ok, _} = send(Request, State),
    close_session(Session, State),
    {reply, ok, State};

handle_call({delete_hss_teluri, User, Phone}, _From, State) ->
    Session = open_session(State),
    Request = em_interface_cai3g_envelopes:delete_ims_teluri(Session, User, Phone),
    {ok, _} = send(Request, State),
    close_session(Session, State),
    {reply, ok, State};

handle_call({update_hss_pass, IMSAssociation}, _From, State) ->
    Session = open_session(State),
    Request = em_interface_cai3g_envelopes:set_ims_pass(Session, IMSAssociation),
    {ok, _} = send(Request, State),
    close_session(Session, State),
    {reply, ok, State};

handle_call({update_hss_phonecontext, IMSAssociation}, _From, State) ->
    Session = open_session(State),
    Request = em_interface_cai3g_envelopes:set_ims_phonecontext(Session, IMSAssociation),
    {ok, _} = send(Request, State),
    close_session(Session, State),
    {reply, ok, State};

    handle_call({create_enum, IMSAssociation}, _From, State) ->
    Session = open_session(State),
    Request = em_interface_cai3g_envelopes:add_ims_enum(Session, IMSAssociation),
    {ok, _} = send(Request, State),
    close_session(Session, State),
    {reply, ok, State};

handle_call({delete_enum, Phone}, _From, State) ->
    Session = open_session(State),
    Request = em_interface_cai3g_envelopes:delete_ims_enum(Session, Phone),
    {ok, _} = send(Request, State),
    close_session(Session, State),
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

open_session(State=#state{ema_user = User, ema_pass = Pass}) ->

    {ok, Session} = em_interface_cai3g_parser:login_response(send(em_interface_cai3g_envelopes:login(User, Pass), State)),
    %?INFO_MSG("EMA session created: ~p", [Session]),
    Session.

close_session(Session, State) ->
    {ok, _} = em_interface_cai3g_parser:logout_response(send(em_interface_cai3g_envelopes:logout(Session), State)).
    %?INFO_MSG("EMA session closed: ~p", [Session]).

send(Request, #state{ema_host = Host}) ->
    %?INFO_MSG("Sending request: ~p", [lists:flatten(Request)]),
  case httpc:request(post,{Host,[],"text/xml",list_to_binary(Request)},[{timeout,1000}],[]) of
    {ok,{{_,200,_}, _Headers, Body}}-> {ok, Body};
    {ok,{{_,500,_}, _Headers, Body}}-> {error, Body};
    {ok,{{_,_OtherStatus,_},_Headers,_Body}}->
      %?LOG("HTTP Status Code unexpected ~p ~n waiting ~p miliseconds ~n",OtherStatus, 5000),
      timer:sleep(5000),
      exit(http_code_unexpected);
    {error,Reason} ->
      %?LOG("Error on sending to EMA ~p ~n waiting ~p miliseconds ~n",Reason, 5000),
      timer:sleep(5000),
      exit(Reason)
  end.
  
% send_test(Request) ->
%      ?INFO_MSG("Sending request: ~p", [lists:flatten(Request)]),
%      {ok, Request}.
