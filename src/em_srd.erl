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

-module(em_srd).
-include("../include/em.hrl").
 
-export([
    create_schema/0,
    add_user/1,
    delete_user/1,
    get_users/1,
    set_e164/1,
    delete_e164/1,
    set_sipuri/1,
    set_sipuri_default/1,
    get_e164/1,
    get_sipuri/1
    ]).

%-record(srd_user, {user_name, user_type, e164, sip_uri, group_id}).

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
        subscriber,
        [{disc_copies, [node()]},
        {type, set},
        {attributes, record_info(fields, subscriber)}]),
        ok.

add_user(#event{user=UserName, group=Group, pubid=PubId}) ->
    User=#subscriber{user=UserName, pubid=PubId, group=Group},
    
    F = fun () ->
        case mnesia:read({subscriber, UserName}) =:= [] of
            true -> mnesia:write(User);
            false -> {error, {activation_error, data_error, 'The user must not already exist in the SRD'}}
        end
    end,
    mnesia:activity(transaction, F),
    {ok, success}.
    

delete_user(UserName) ->
    F = fun () ->
        case mnesia:read({subscriber, UserName}) =/= [] of
            true -> mnesia:delete({subscriber, UserName});
            false -> {error, {termination_error, data_error, 'The user must exist in the SRD'}}
        end
    end,
    mnesia:activity(transaction, F),
    {ok, success}.

set_e164(#event{user=UserName, phone=Phone})->
    F = fun () ->
        case mnesia:read({subscriber, UserName}) =:= [] of
            true -> undefined;
            false ->
                [R] = mnesia:wread({subscriber, UserName}),
                mnesia:write(R#subscriber{phone = Phone})
        end
    end,
    mnesia:activity(transaction, F).
    
delete_e164(UserId)->
    F = fun () ->
        case mnesia:read({subscriber, UserId}) =:= [] of
            true -> undefined;
            false ->
                [R] = mnesia:wread({subscriber, UserId}),
                mnesia:write(R#subscriber{phone = undefined})
        end
    end,
    mnesia:activity(transaction, F).

get_e164(UserId) ->
    F = fun() ->
        case mnesia:read({subscriber, UserId}) of  
            [#subscriber{phone=E}] -> E;
            [] -> undefined
        end
    end,
    mnesia:activity(transaction, F).

get_sipuri(UserId) ->
    F = fun() ->
        case mnesia:read({subscriber, UserId}) of 
            [#subscriber{pubid=S}] -> S;
            [] -> undefined
        end
    end,
    mnesia:activity(transaction, F).

set_sipuri(#event{user=UserName, pubid=PubId})->
    F = fun () ->
        case mnesia:read({subscriber, UserName}) =:= [] of
            true -> undefined;
            false ->
                [R] = mnesia:wread({subscriber, UserName}),
                mnesia:write(R#subscriber{pubid = PubId})
        end
    end,
    mnesia:activity(transaction, F).
    
set_sipuri_default(UserId)->
    F = fun () ->
        case mnesia:read({subscriber, UserId}) =:= [] of
            true -> undefined;
            false ->
                [R] = mnesia:wread({subscriber, UserId}),
                mnesia:write(R#subscriber{pubid = UserId})
        end
    end,
    mnesia:activity(transaction, F).

get_users(GrpId) ->
    F = fun() ->
        Result = '$1',
        User = #subscriber{user = '$1', group = GrpId, _ = '_'},
        mnesia:select(subscriber, [{User, [], [Result]}])
    end,
    mnesia:activity(transaction, F).
    
%%%===================================================================
%%% Internal functions
%%%===================================================================