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
 
-export([
    create_schema/0,
    add_user/3,
    delete_user/1,
    get_users/1,
    set_e164/2,
    set_sipuri/2,
    get_e164/1,
    get_sipuri/1
    ]).

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
    {atomic, ok} = mnesia:create_table(
        srd_user,
        [{disc_copies, [node()]},
        {type, set},
        {attributes, record_info(fields, srd_user)}]),
        ok.

add_user(UserName, GrpId, UserType)->
    User = #srd_user{user_name = UserName, user_type = UserType, group_id = GrpId},
    F = fun () ->
        case mnesia:read({srd_user, UserName}) =:= [] of
            true -> mnesia:write(User);
            false -> exists
        end
    end,
    mnesia:activity(transaction, F).

delete_user(UserId)->
    F = fun () ->
        case mnesia:read({srd_user, UserId}) =/= [] of
            true -> mnesia:delete({srd_user, UserId});
            false -> undefined
        end
    end,
    mnesia:activity(transaction, F).

set_e164(UserId, E164)->
    F = fun () ->
        case mnesia:read({srd_user, UserId}) =:= [] of
            true -> undefined;
            false ->
                [R] = mnesia:wread({srd_user, UserId}),
                mnesia:write(R#srd_user{e164 = E164})
        end
    end,
    mnesia:activity(transaction, F).

get_e164(UserId) ->
    F = fun() ->
        case mnesia:read({srd_user, UserId}) of  
            [#srd_user{e164=E}] -> E;
            [] -> undefined
        end
    end,
    mnesia:activity(transaction, F).

get_sipuri(UserId) ->
    F = fun() ->
        case mnesia:read({srd_user, UserId}) of 
            [#srd_user{sip_uri=S}] -> S;
            [] -> undefined
        end
    end,
    mnesia:activity(transaction, F).

set_sipuri(UserId, SipUri)->
    F = fun () ->
        case mnesia:read({srd_user, UserId}) =:= [] of
            true -> undefined;
            false ->
                [R] = mnesia:wread({srd_user, UserId}),
                mnesia:write(R#srd_user{sip_uri = SipUri})
        end
    end,
    mnesia:activity(transaction, F).

get_users(GrpId) ->
    F = fun() ->
        Result = '$1',
        User = #srd_user{user_name = '$1', group_id = GrpId, _ = '_'},
        mnesia:select(srd_user, [{User, [], [Result]}])
    end,
    mnesia:activity(transaction, F).
    
%%%===================================================================
%%% Internal functions
%%%===================================================================