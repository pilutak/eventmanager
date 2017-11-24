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


%% API
-export([login_response/1,logout_response/1]).
-import(em_utils,[get_elements/2,get_element_name/1,get_element_childs/1,get_element_text/1]).

login_response({error,Reason})->{error,Reason};
login_response({ok,Response})->
  {Response_parsed,_} = xmerl_scan:string(Response),
  case is_successful(Response_parsed) of
    true  ->
      [Body]=get_elements('S:Body',get_element_childs(Response_parsed)),
      [LoginResponse]=get_elements('LoginResponse',get_element_childs(Body)),
      [SessionID]=get_elements('sessionId',get_element_childs(LoginResponse)),
      Resp_SessionID=get_element_text(SessionID),
      {ok,Resp_SessionID};
    Fail -> Fail
  end.


logout_response({error,Reason})->{error,Reason};
logout_response({ok,Response})->
  {Response_parsed,_} = xmerl_scan:string(Response),
  case is_successful(Response_parsed) of
    true  ->
      [Header]=get_elements('S:Header',get_element_childs(Response_parsed)),
      [SessionID]=get_elements('SessionId',get_element_childs(Header)),
      Resp_SessionID=get_element_text(SessionID),
      {ok,Resp_SessionID};
    Fail -> Fail
  end.


%%%===================================================================
%%% Internal functions
%%%===================================================================

is_successful(Response_parsed)->
  [Body]=get_elements('S:Body',get_element_childs(Response_parsed)),
  case get_elements('S:Fault',get_element_childs(Body)) of
    [undefined] -> true;
    [Fault]  ->
      [FaultCode] = get_elements('faultcode',get_element_childs(Fault)),
      Resp_FaultCode = get_element_text(FaultCode),
      [FaultString] = get_elements('faultstring',get_element_childs(Fault)),
      Resp_FaultString = get_element_text(FaultString),
      {error,{faultcode,Resp_FaultCode},{faultstring,Resp_FaultString}}
  end.



