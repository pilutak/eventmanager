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
    create_user/2,
    delete_user/2,
    get_users/1,
    set_phone/2,
    delete_phone/1,
    set_sipuri/2,
    get_phone/1,
    get_sipuri/1,
    get_group/1,
    get_pass/1,
    set_pass/2,
    set_vmail/3,
    delete_vmail/1,
    get_vmail_user/1,
    get_vmail_pass/1,
    get_phonecontext/1,
    set_phonecontext/2,
    get_type/1,
    user_exists/1
    ]).


%%%===================================================================
%%% API
%%%===================================================================
create_user(User, Attrs) ->
    PubId = maps:get(pubid, Attrs, User),
    GroupId = maps:get(group, Attrs),
    Type = maps:get(type, Attrs),
    AssociationId = em_utils:md5_hex(User),
    PhoneContext = maps:get(phonecontext, Attrs, "tg.gl"),
    C = connect(),
    {ok, _} = epgsql:equery(C, "insert into srd_user (id, group_id, sipurl, user_type, phonecontext, association_id, created) values ($1,$2,$3,$4,$5,$6, timezone('utc', now()))", [User,GroupId,PubId,Type,PhoneContext,AssociationId]),
    epgsql:close(C).

delete_user(User, _Attrs) ->
    C = connect(),
    {ok, _} = epgsql:equery(C, "delete from srd_user where id=$1", [User]),
    epgsql:close(C).

get_phone(User) ->
    C = connect(),
    {ok, _, Rows} = epgsql:equery(C, "select trim(e164) from srd_user where id= $1", [User]),
    ok = epgsql:close(C),
    case Rows of
        [] -> undefined;
        _ -> [{R}] = Rows,
              case R of
                  null -> undefined;
                  _-> binary_to_list(R)
             end
    end.
    
set_phone(User, Attrs) ->
    Phone = maps:get(phone, Attrs),
    C = connect(),
    {ok, _} = epgsql:equery(C, "update srd_user set e164=$1, updated=timezone('utc', now()) where id=$2", [Phone, User]),
    epgsql:close(C).

delete_phone(User)->
    C = connect(),
    {ok, _} = epgsql:equery(C, "update srd_user set e164=NULL, updated=timezone('utc', now()) where id=$1", [User]),
    epgsql:close(C).
        
get_sipuri(User) ->
    C = connect(),
    {ok, _, Rows} = epgsql:equery(C, "select trim(sipurl) from srd_user where id= $1", [User]),
    ok = epgsql:close(C),
    case Rows of
        [] -> undefined;
        _ -> [{R}] = Rows,
              case R of
                  null -> undefined;
                  _-> binary_to_list(R)
             end
    end.

set_sipuri(User, Attrs)->
    PubId = maps:get(pubid, Attrs, User),
    C = connect(),
    {ok, _} = epgsql:equery(C, "update srd_user set sipurl=$1, updated=timezone('utc', now()) where id=$2", [PubId, User]),
    epgsql:close(C). 
       
get_group(GroupId) ->
    C = connect(),
    {ok, _, Rows} = epgsql:equery(C, "select trim(group_id) from srd_user where id= $1", [GroupId]),
    ok = epgsql:close(C),
    case Rows of
        [] -> undefined;
        _ -> [{R}] = Rows,
              case R of
                  null -> undefined;
                  _-> binary_to_list(R)
             end
    end.

get_pass(UserId) ->
    C = connect(),
    {ok, _, Rows} = epgsql:equery(C, "select trim(password) from srd_migration_passwords where id= $1", [UserId]),
    ok = epgsql:close(C),
    case Rows of
        [] -> undefined;
        _ -> [{R}] = Rows,
              case R of
                  null -> undefined;
                  _-> binary_to_list(R)
             end
    end.

set_pass(UserId, Pass)->
    C = connect(),
    {ok, _} = epgsql:equery(C, "update srd_user set password=$1, updated=timezone('utc', now()) where id=$2", [Pass,UserId]),
    ok = epgsql:close(C).
    
get_users(GrpId) ->
    C = connect(),
    {ok, _, Rows} = epgsql:equery(C, "select trim(id) from srd_user where group_id=$1", [GrpId]),
    ok = epgsql:close(C),
    Rows.
    
set_vmail(UserName, MailUser, MailPass) ->
    C = connect(),
    {ok, _} = epgsql:equery(C, "update srd_user set vmail_user=$1, vmail_password=$2, updated=timezone('utc', now()) where id=$3", [MailUser,MailPass,UserName]),
    epgsql:close(C).
        
delete_vmail(UserId)->
    C = connect(),
    {ok, _} = epgsql:equery(C, "update srd_user set vmail_user='NODATA', vmail_password='NODATA', updated=timezone('utc', now()) where id=$1", [UserId]),
    epgsql:close(C).
    
get_vmail_user(UserId) ->
    C = connect(),
    {ok, _, Rows} = epgsql:equery(C, "select trim(vmail_user) from srd_user where id=$1", [UserId]),
    ok = epgsql:close(C),
    case Rows of
        [] -> undefined;
        _ -> [{R}] = Rows,
              case R of
                  null -> undefined;
                  _-> binary_to_list(R)
             end
    end.

get_vmail_pass(UserId) ->
    C = connect(),
    {ok, _, Rows} = epgsql:equery(C, "select trim(vmail_password) from srd_user where id=$1", [UserId]),
    ok = epgsql:close(C),
    case Rows of
        [] -> undefined;
        _ -> [{R}] = Rows,
              case R of
                  null -> undefined;
                  _-> binary_to_list(R)
             end
    end.

get_phonecontext(UserId) ->
    C = connect(),
    {ok, _, Rows} = epgsql:equery(C, "select trim(phonecontext) from srd_user where id=$1", [UserId]),
    ok = epgsql:close(C),
    case Rows of
        [] -> undefined;
        _ -> [{R}] = Rows,
              case R of
                  null -> undefined;
                  _-> binary_to_list(R)
             end
    end.
    
set_phonecontext(UserId, PhoneContext)->
    C = connect(),
    {ok, _} = epgsql:equery(C, "update srd_user set phonecontext=$1, updated=timezone('utc', now()) where id=$2", [PhoneContext,UserId]),
    epgsql:close(C).
    
get_type(IMSAssociation) ->
    Id = maps:get(user, IMSAssociation),
    C = connect(),
    {ok, _, Rows} = epgsql:equery(C, "select trim(user_type) from srd_user where id= $1", [Id]),
    ok = epgsql:close(C),
    case Rows of
        [] -> undefined;
        _ -> [{R}] = Rows,
             binary_to_list(R)
    end.

user_exists(UserId) ->
    C = connect(),
    {ok, _, Rows} = epgsql:equery(C, "select exists(select 1 from srd_user where id=$1)", [UserId]),
    ok = epgsql:close(C),
    case Rows of
        [{false}] -> false;
        [{true}] -> true
    end.        
%%%===================================================================
%%% Internal functions
%%%===================================================================
connect() ->
    Hostname = econfig:get_value(em, "srd_db", "srd_db_host"),
    Database = econfig:get_value(em, "srd_db", "srd_db_database"),
    Username = econfig:get_value(em, "srd_db", "srd_db_username"),
    Password = econfig:get_value(em, "srd_db", "srd_db_password"),
    
    {ok, C} = epgsql:connect(Hostname, Username, Password, [{database, Database},{timeout, 4000}]),
    C.
