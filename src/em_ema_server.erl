%%%-------------------------------------------------------------------
%%% @Author Thomas Elsgaard <thomas.elsgaard@timezone4.com>
%%% @copyright (C) 2018, Thomas Elsgaard
%%% @doc
%%%
%%% @end
%%% Created : 15 Jun 2018 by Thomas Elsgaard <thomas.elsgaard@timezone4.com>
%%%-------------------------------------------------------------------
-module(em_ema_server).

-behaviour(gen_server).
-include_lib("xmerl/include/xmerl.hrl"). 

%% API
-export([start_link/0, close_session/1, sync_command/3, process_fault/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-define(PORT, "8080").
-define(URL, "/CAI3G1.2/services/CAI3G1.2").
-define(NS1, "http://schemas.xmlsoap.org/soap/envelope/").
-define(NS2, "http://schemas.ericsson.com/cai3g1.2/").

-record(state, {session, host}).


%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link(?MODULE, [], []).
    
close_session(C) when is_pid(C) ->
    catch gen_server:cast(C, stop),
    ok.

sync_command(C, Command, Args) -> 
        gen_server:call(C, {Command, Args}).

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
    ok = application:ensure_started(inets),
    {ok, #state{}}.

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
handle_call({start_session, {Host, User, Pass}}, _From, State) ->
    case open_session(Host, User, Pass) of
        {ok, SessionId} -> {reply, {ok, SessionId}, State#state{session = SessionId, host = Host}};
        {error, Resp} -> {reply, {error, Resp}, State}
    end;
    
handle_call({command, Args}, _From, State=#state{session = SessionId, host = Host}) ->
    Cmd = serialize(SessionId, Args),
    Reply = send_request(Host, Cmd),
    {reply, Reply, State};

handle_call(terminate, _From, State=#state{session = SessionId, host = Host}) ->
    Cmd = {'cai3:Logout',[],[{'cai3:sessionId',[SessionId]}]},
    Reply = send_request(Host, serialize(SessionId, Cmd)),
    {stop, normal, Reply, State};

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
handle_cast(stop, State) ->
    {stop, normal, State};


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
serialize(MO) ->
    Envelope = {'S:Envelope', namespaces() ,[{'S:Header',[],[]}, {'S:Body', [],[MO]}]},
    lists:flatten(xmerl:export_simple([Envelope], xmerl_xml)). 
    
serialize(SessionId, MO) ->
    Envelope = {'S:Envelope', namespaces() ,[{'S:Header',[],[{'cai3:SessionId',[SessionId]}]}, {'S:Body', [],[MO]}]},
    lists:flatten(xmerl:export_simple([Envelope], xmerl_xml)). 
                 
send_request(Host, Request) ->
    case httpc:request(post,{uri(Host),[], "text/xml", Request},[{timeout,2000}],[]) of
        {ok,{{_,200,_}, _Headers, Body}} -> process_success(Body);
        {ok,{{_,500,_}, _Headers, Body}} -> process_fault(Body);    
        {ok,{{_,_OtherStatus,_}, _Headers, _Body}} ->
            exit(http_code_unexpected);
        {error, {failed_connect, Error}} ->
            {error, Error};
        {error, Reason} ->
            exit(Reason)
    end. 
  
process_success(Body) ->
    {Envelope, _} = xmerl_scan:string(Body),
    [Body1] = xmerl_xpath:string("//S:Body", Envelope),
    [Payload] = [E || E <- Body1#xmlElement.content, is_record(E, xmlElement)],
    {ok, Payload}.
    
process_fault(Body) ->
    {XMLContent, _} = xmerl_scan:string(Body),
    [Body1] = xmerl_xpath:string("//S:Body", XMLContent),    
    [Ns2Fault] = [E || E <- Body1#xmlElement.content, is_record(E, xmlElement)],
    [_,_,Detail] = [E || E <- Ns2Fault#xmlElement.content, is_record(E, xmlElement)],
    FCode = val(xmerl_xpath:string("//faultcode", Detail)),
    ECode = val(xmerl_xpath:string("//errorcode", Detail)),
    RCode = val(xmerl_xpath:string("//respCode", Detail)),
    
    FaultCode = format_faultcode(FCode),
    ERCode = select_ercode(ECode, RCode),
    {error,{FaultCode, ERCode}}.     
   
%sessionid(Payload) ->
%    val(xmerl_xpath:string("//sessionId", Payload)).
    
uri(Host) ->
    "http://" ++ Host ++ ":" ++ ?PORT ++ ?URL.
    
namespaces() ->
    [{'xmlns:S', ?NS1},{'xmlns:cai3', ?NS2}].

val([]) ->
    undefined;
val(X) ->
    [#xmlElement{name = _N, content = [#xmlText{value = V}|_]}] = X,
    V.

select_ercode(undefined, undefined) ->
    undefined;
select_ercode(ErrorCode, undefined) ->
    list_to_integer(ErrorCode);
select_ercode(undefined, RespCode) ->
    list_to_integer(RespCode).

format_faultcode(undefined) ->
    undefined;
format_faultcode(FaultCode) ->
    list_to_integer(FaultCode).

open_session(Host, User, Pass) ->
    Cmd = {'cai3:Login',[],[
            {'cai3:userId',[User]}, 
            {'cai3:pwd',[Pass]}]},
                
    case send_request(Host, serialize(Cmd)) of
        {ok, Resp} -> get_session(Resp);
        Error -> Error
    end.
    
get_session(Body) ->
    case val(xmerl_xpath:string("//sessionId", Body)) of
        undefined -> {error, "SessionId not found in response"};
        SessionId -> {ok, SessionId}
    end.
    
                

%%%===================================================================
%%% Unit Tests
%%%===================================================================
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

serialize_test_() ->
    {ok, XML1} = file:read_file("test/user_create_req.xml"),
    XML2 = binary_to_list(XML1),
    [?_assert(serialize(sessionid(), dummy_user_mo()) =:= XML2)].


fault_resp_test_() ->
    {ok, XML1} = file:read_file("test/user_create_error.xml"),
    {ok, XML3} = file:read_file("test/user_login_error.xml"),
    
    XML2 = binary_to_list(XML1),
    XML4 = binary_to_list(XML3),
    
    
    [?_assert(process_fault(XML2) =:= {error,{4006,13004}}),
    ?_assert(process_fault(XML4) =:= {error,{3014,1004}})].

        
dummy_user_mo() ->    
    Ns1 = "http://schemas.ericsson.com/ma/HSS/",
    MoType = "IMSAssociation@http://schemas.ericsson.com/ma/HSS/",
    MoId = "test@test.com",
    {'cai3:Create', [{'xmlns:hss', Ns1}], [
        {'cai3:MOType',[MoType]},
        {'cai3:MOId', [], [
            {'hss:associationId', [], [MoId]}]},
        {'cai3:MOAttributes',[], [
            {'hss:CreateIMSAssociation', [{'associationId', MoId}],[
                    {'hss:associationId',[MoId]}]}]}]}.
                    
sessionid() ->
    "963983366d244eab80232b589be72e39".
-endif.       
