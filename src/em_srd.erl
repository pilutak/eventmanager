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
    add_user/1,
    delete_user/1,
    get_users/1,
    set_e164/1,
    delete_e164/1,
    set_sipuri/1,
    set_sipuri_default/1,
    get_e164/1,
    get_sipuri/1,
    get_group/1,
    user_exists/1,
    get_type/1,
    set_pass/2,
    set_vmail/3,
    delete_vmail/1,
    get_vmail_user/1,
    get_vmail_pass/1,
    get_phonecontext/1,
    set_phonecontext/2
    ]).


%%%===================================================================
%%% API
%%%===================================================================

add_user(#event{user=U, group=G, pubid=P, type=T}) ->
    C = connect(),
    {ok, _} = epgsql:equery(C, "insert into srd_user (id, group_id, sipurl, user_type, phonecontext) values ($1,$2,$3,$4,'tg.gl')", [U,G,P,T]),
    ok = epgsql:close(C),
    {ok, success}.
    
delete_user(UserName) ->
    C = connect(),
    {ok, _} = epgsql:equery(C, "delete from srd_user where id=$1", [UserName]),
    ok = epgsql:close(C),
    {ok, success}.    
    
set_e164(#event{user=U, phone=P})->
    C = connect(),
    {ok, _} = epgsql:equery(C, "update srd_user set e164=$1 where id=$2", [P,U]),
    ok = epgsql:close(C),
    {ok, success}.
    
delete_e164(UserId)->
    C = connect(),
    {ok, _} = epgsql:equery(C, "update srd_user set e164='NODATA' where id=$1", [UserId]),
    ok = epgsql:close(C),
    {ok, success}.

get_e164(UserId) ->
    C = connect(),
    {ok, _, Rows} = epgsql:equery(C, "select trim(e164) from srd_user where id= $1", [UserId]),
    ok = epgsql:close(C),
    Rows.

get_sipuri(UserId) ->
    C = connect(),
    {ok, _, Rows} = epgsql:equery(C, "select trim(sipurl) from srd_user where id= $1", [UserId]),
    ok = epgsql:close(C),
    Rows.

set_sipuri(#event{user=U, pubid=P})->
    C = connect(),
    {ok, _} = epgsql:equery(C, "update srd_user set sipurl=$1 where id=$2", [P,U]),
    ok = epgsql:close(C),
    {ok, success}.
    
set_sipuri_default(UserId)->
    C = connect(),
    {ok, _} = epgsql:equery(C, "update srd_user set sipurl=$1 where id=$1", [UserId]),
    ok = epgsql:close(C),
    {ok, success}.
    
get_group(UserId) ->
    C = connect(),
    {ok, _, Rows} = epgsql:equery(C, "select trim(group_id) from srd_user where id= $1", [UserId]),
    ok = epgsql:close(C),
    Rows.
    
set_pass(UserId, Pass)->
    C = connect(),
    {ok, _} = epgsql:equery(C, "update srd_user set password=$1 where id=$2", [Pass,UserId]),
    ok = epgsql:close(C),
    {ok, success}.
    
user_exists(UserId) ->
    C = connect(),
    {ok, _, Rows} = epgsql:equery(C, "select exists(select 1 from srd_user where id=$1)", [UserId]),
    ok = epgsql:close(C),
    
    case Rows of
        [{false}] -> false;
        
        [{true}] -> true
    end.
    
get_type(UserId) ->
    C = connect(),
    {ok, _, Rows} = epgsql:equery(C, "select trim(user_type) from srd_user where id= $1", [UserId]),
    ok = epgsql:close(C),
    Rows.
    
get_users(GrpId) ->
    C = connect(),
    {ok, _, Rows} = epgsql:equery(C, "select trim(id) from srd_user where group_id=$1", [GrpId]),
    ok = epgsql:close(C),
    Rows.
    
set_vmail(UserName, MailUser, MailPass) ->
    C = connect(),
    {ok, _} = epgsql:equery(C, "update srd_user set vmail_user=$1, vmail_password=$2 where id=$3", [MailUser,MailPass,UserName]),
    ok = epgsql:close(C),
    {ok, success}.
    
delete_vmail(UserId)->
    C = connect(),
    {ok, _} = epgsql:equery(C, "update srd_user set vmail_user='NODATA', vmail_password='NODATA' where id=$1", [UserId]),
    ok = epgsql:close(C),
    {ok, success}.   
    
get_vmail_user(UserId) ->
    C = connect(),
    {ok, _, Rows} = epgsql:equery(C, "select trim(vmail_user) from srd_user where id=$1", [UserId]),
    ok = epgsql:close(C),
    Rows.

get_vmail_pass(UserId) ->
    C = connect(),
    {ok, _, Rows} = epgsql:equery(C, "select trim(vmail_password) from srd_user where id=$1", [UserId]),
    ok = epgsql:close(C),
    Rows.

get_phonecontext(UserId) ->
    C = connect(),
    {ok, _, Rows} = epgsql:equery(C, "select phonecontext from srd_user where id=$1", [UserId]),
    ok = epgsql:close(C),
    Rows.
    
set_phonecontext(UserId, PhoneContext)->
    C = connect(),
    {ok, _} = epgsql:equery(C, "update srd_user set phonecontext=$1 where id=$2", [PhoneContext,UserId]),
    ok = epgsql:close(C),
    {ok, success}.
    
%%%===================================================================
%%% Internal functions
%%%===================================================================
connect() ->
        {ok, C} = epgsql:connect("localhost", "srd", "mysecretpassword",
        [{database, "srd"},{timeout, 4000}]),
        C.