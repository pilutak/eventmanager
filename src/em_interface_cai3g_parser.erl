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

-module(em_interface_cai3g_parser).

-export([
    login_response/1,
    logout_response/1,
    send_response/1,
    error_resp/1]).

%%%===================================================================
%%% API
%%%===================================================================
    
login_response({error,Reason}) -> {error,Reason};
login_response({ok, Resp}) ->
    {RespParsed, _} = xmerl_scan:string(Resp),
    case is_successful(RespParsed) of
        true  ->
            [Body] = em_utils:get_elements('S:Body', em_utils:get_element_childs(RespParsed)),
            [LoginResp] = em_utils:get_elements('LoginResponse', em_utils:get_element_childs(Body)),
            [Session] = em_utils:get_elements('sessionId', em_utils:get_element_childs(LoginResp)),
            RespSession = em_utils:get_element_text(Session),
            {ok,RespSession};
        Fail -> Fail
    end.


logout_response({error,Reason}) -> {error,Reason};
logout_response({ok,Resp}) ->
    {RespParsed,_} = xmerl_scan:string(Resp),
    case is_successful(RespParsed) of
        true  ->
            [Header] = em_utils:get_elements('S:Header', em_utils:get_element_childs(RespParsed)),
            [Session]= em_utils:get_elements('SessionId', em_utils:get_element_childs(Header)),
            RespSession = em_utils:get_element_text(Session),
            {ok,RespSession};
        Fail -> Fail
    end.
    
send_response({ok, Resp}) ->
    {RespParsed, _} = xmerl_scan:string(Resp),

    case is_send_successful(RespParsed) of
        true  -> {ok, Resp};
        Fail -> Fail
    end.

error_resp(Resp) ->
    {RespParsed, _} = xmerl_scan:string(Resp),

    case is_send_successful(RespParsed) of
        true  -> {ok, Resp};
        Fail -> Fail
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

is_successful(RespParsed) ->
    [Body] = em_utils:get_elements('S:Body', em_utils:get_element_childs(RespParsed)),
    case em_utils:get_elements('S:Fault', em_utils:get_element_childs(Body)) of
        [undefined] -> true;
        [Fault] -> 
            [FaultCode] = em_utils:get_elements('faultcode', em_utils:get_element_childs(Fault)),
            RespFaultCode = em_utils:get_element_text(FaultCode),
            [FaultString] = em_utils:get_elements('faultstring', em_utils:get_element_childs(Fault)),
            RespFaultString = em_utils:get_element_text(FaultString),
            {error,{faultcode,RespFaultCode},{faultstring,RespFaultString}}
    end.

is_send_successful(RespParsed) ->
    [Body] = em_utils:get_elements('S:Body', em_utils:get_element_childs(RespParsed)),
    case em_utils:get_elements('ns2:Fault', em_utils:get_element_childs(Body)) of
        [undefined] -> true;
        [Fault] -> 
            [Detail] = em_utils:get_elements('detail', em_utils:get_element_childs(Fault)),
            [Cai3gFault] = em_utils:get_elements('Cai3gFault:Cai3gFault', em_utils:get_element_childs(Detail)),
            [FaultCode] = em_utils:get_elements('faultcode', em_utils:get_element_childs(Cai3gFault)),
            [Details] = em_utils:get_elements('details', em_utils:get_element_childs(Cai3gFault)),
            [IMSFault] = em_utils:get_elements('IMSFault:IMSFault', em_utils:get_element_childs(Details)),
            [ErrorCode] = em_utils:get_elements('errorcode', em_utils:get_element_childs(IMSFault)),
            
            RespFaultCode = em_utils:get_element_text(FaultCode),
            RespErrorCode = em_utils:get_element_text(ErrorCode),
            
            {error, {RespFaultCode, RespErrorCode}}
    end.
