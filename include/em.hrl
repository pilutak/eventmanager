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

%% Serviceprofiles must be changes to match the HSS configuration
%% valid profiles on ICS5:
%%IMT_VIRTUAL
%%IMT_VIRTUAL_csas02
%%IMS_CENTREX
%%IMS_CENTREX_MAE
%%IMS_CENTREX_csas02
%%BusinessTrunk
%%BusinessTrunk_wild

-define(SERVICEPROFILE, 'IMT_VIRTUAL_csas02').
-define(CENTREXPROFILE, 'IMS_CENTREX_csas02').
-define(TRUNKPROFILE_DDI, 'BusinessTrunk_wild').
-define(SIPTRUNKPROFILE_PILOT, 'BusinessTrunk').

-define(ERROR_MSG(Format, Args),
	error_logger:error_msg("(~p:~p:~p) " ++ Format,
			       [self(), ?MODULE, ?LINE | Args])).

-define(INFO_MSG(Format, Args),
	error_logger:info_msg("(~p:~p:~p) " ++ Format,
			       [self(), ?MODULE, ?LINE | Args])).
                     
-define(VMAIL_URL,
  case application:get_env(em, vmail) of
    undefined -> "http://localhost:7026";
    {ok,{URL, _User, _Pass}} -> URL
  end).

-define(VMAIL_USER,
  case application:get_env(em, vmail) of
    undefined -> "user";
    {ok,{_URL, User, _Pass}} -> User
  end).

-define(VMAIL_PASS,
  case application:get_env(em, vmail) of
    undefined -> "password";
    {ok,{_URL, _User, Pass}} -> Pass
  end).