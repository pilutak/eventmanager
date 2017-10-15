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

-module(em_db).
 
%% API
-export([create_schema/0]).
-export([add_user/2]).
-export([delete_user/1]).
-export([get_users/0]).


-record(srd_user, {user_name, user_type, e164, sip_uri, group_id}).


%%%===================================================================
%%% API
%%%===================================================================

create_schema() ->
	  application:set_env(mnesia, dir, "/var/lib/eventmanager"),
    mnesia:create_schema([node()]),
    application:ensure_all_started(mnesia),
    ok = create_tables(),
    mnesia:backup("FALLBACK.BUP"),
    mnesia:install_fallback("FALLBACK.BUP"),
    application:stop(mnesia).

create_tables() ->
   {atomic, ok} =
        mnesia:create_table(
          srd_user,
          [{disc_copies, [node()]},
           {type, bag},
           {attributes, record_info(fields, srd_user)}]),
	ok.


add_user(UserName, UserType)->
	User = #srd_user{user_name = UserName, user_type = UserType},
	F = fun () ->
			mnesia:write(User)
		end,
    mnesia:activity(transaction, F).


delete_user(UserId)->
  F = fun () ->
    case mnesia:read({srd_user, UserId}) =/= [] of
      true ->
        mnesia:delete({srd_user, UserId});
      false ->
        undefined
    end
  end,  
    mnesia:activity(transaction, F).


 get_users() ->
  F = fun() ->
    Result = ['$1','$2', '$3', '$4', '$5'],
    User = #srd_user{user_name = '$1', user_type = '$2', e164 = '$3', sip_uri = '$4', group_id = '$5' },
    mnesia:select(srd_user, [{User, [], [Result]}])
  end,
  mnesia:activity(transaction, F).   
