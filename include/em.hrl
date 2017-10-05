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

-define(LOG(X),error_logger:info_msg(X,[])).
-define(LOG(X,Y),error_logger:info_msg(X,[Y])).
-define(LOG(X,Y,Z),error_logger:info_msg(X,[Y,Z])).

-define(RECONNECT_DELAY,
  case application:get_env(reconnect_delay) of
    undefined -> 5000; %% By default 5 sec
    {ok,Val} -> Val
  end).

-define(BW_CONNECTION_TIMEOUT,
  case application:get_env(bw_connection_timeout) of
    undefined -> 35000;
    {ok,Val} -> Val
  end).

-define(EMA_CONNECTION_TIMEOUT,
  case application:get_env(ema_connection_timeout) of
    undefined -> 5000;
    {ok,Val} -> Val
  end).

-define(EMA_URL,
  case application:get_env(ema) of
    undefined -> "http://localhost";
    {ok,[{URL,_UserId,_Pass}]} -> URL
  end).

-define(EMA_UserID,
  case application:get_env(ema) of
    undefined -> "userId";
    {ok,[{_URL,UserId,_Pass}]} -> UserId
  end).

-define(EMA_Pass,
  case application:get_env(ema) of
    undefined -> "password123456";
    {ok,[{_URL,_UserId,Pass}]} -> Pass
  end).