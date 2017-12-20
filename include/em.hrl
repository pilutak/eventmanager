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

-record(subscriber, {user, pass, pubid, phone, sprofile, csprofile, type, ispsi, irs, isdefault, group}).
-record(event, {user, pass, pubid, phone, group, csprofile, ispsi, irs, isdefault, sprofile}).

-record(state, { session }).

-define(ERROR_MSG(Format, Args),
	error_logger:error_msg("(~p:~p:~p) " ++ Format,
			       [self(), ?MODULE, ?LINE | Args])).

-define(INFO_MSG(Format, Args),
	error_logger:info_msg("(~p:~p:~p) " ++ Format,
			       [self(), ?MODULE, ?LINE | Args])).
                   
-define(EMA_URL,
  case application:get_env(em, ema) of
    undefined -> "http://localhost:8998";
    {ok,{URL, _User, _Pass}} -> URL
  end).

-define(EMA_USER,
  case application:get_env(em, ema) of
    undefined -> "sogadm";
    {ok,{_URL, User, _Pass}} -> User
  end).

-define(EMA_PASS,
  case application:get_env(em, ema) of
    undefined -> "sogadm";
    {ok,{_URL, _User, Pass}} -> Pass
  end).