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
    create_user/1,
    insert_event/3,
    get_events/0,
    complete_event/1,
    fail_event/1,
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

create_user(IMSAssociation) ->
    Id = maps:get(user, IMSAssociation),
    PubId = Id,
    GroupId = maps:get(group, IMSAssociation),
    Type = maps:get(type, IMSAssociation),
    AssociationId = maps:get(association, IMSAssociation),
    
    C = connect(),
    {ok, _} = epgsql:equery(C, "insert into srd_user (id, group_id, sipurl, user_type, phonecontext, association_id, created) values ($1,$2,$3,$4,'tg.gl',$5, current_timestamp)", [Id,GroupId,PubId,Type,AssociationId]),
    epgsql:close(C).

insert_event(UserId, Command, Event) ->    
    C = connect(),
    {ok, _, _, Rows} = epgsql:equery(C, "insert into srd_event (user_id, command, event, status, inserted) values ($1,$2,$3,$4, current_timestamp) returning id", [UserId, Command, Event, "pending"]),
    epgsql:close(C),
    case Rows of
        [] -> undefined;
        _ -> [{R}] = Rows,
             R
             %binary_to_list(R)
    end.

get_events() ->
    C = connect(),
    {ok, _, Rows} = epgsql:equery(C, "select id, user_id, command from srd_event where true", []),
    ok = epgsql:close(C),    
    case Rows of
        [] -> undefined;
        _ -> Rows,
            ?INFO_MSG("Rows: ~p~n", [Rows]), 
            [event_to_json(P) || P <- Rows]
    end.



complete_event(Id) ->
    C = connect(),
    {ok, _} = epgsql:equery(C, "update srd_event set status=$1 where id=$2", ["completed",Id]),
    epgsql:close(C).

fail_event(Id) ->
    C = connect(),
    {ok, _} = epgsql:equery(C, "update srd_event set status=$1 where id=$2", ["failed",Id]),
    epgsql:close(C).


    
delete_user(IMSAssociation) ->
    Id = maps:get(user, IMSAssociation),
    C = connect(),
    {ok, _} = epgsql:equery(C, "delete from srd_user where id=$1", [Id]),
    epgsql:close(C).
        
set_e164(IMSAssociation) ->
    Id = maps:get(user, IMSAssociation),
    Phone = maps:get(phone, IMSAssociation),
    C = connect(),
    {ok, _} = epgsql:equery(C, "update srd_user set e164=$1, updated=current_timestamp where id=$2", [Phone,Id]),
    epgsql:close(C).
        
delete_e164(Id)->
    C = connect(),
    {ok, _} = epgsql:equery(C, "update srd_user set e164='NODATA', updated=current_timestamp where id=$1", [Id]),
    epgsql:close(C).
    
get_e164(IMSAssociation) ->
    Id = maps:get(user, IMSAssociation),
    C = connect(),
    {ok, _, Rows} = epgsql:equery(C, "select trim(e164) from srd_user where id= $1", [Id]),
    ok = epgsql:close(C),
    case Rows of
        [] -> undefined;
        _ -> [{R}] = Rows,
             binary_to_list(R)
    end.
    
get_sipuri(IMSAssociation) ->
    Id = maps:get(user, IMSAssociation),
    C = connect(),
    {ok, _, Rows} = epgsql:equery(C, "select trim(sipurl) from srd_user where id= $1", [Id]),
    ok = epgsql:close(C),
    case Rows of
        [] -> undefined;
        _ -> [{R}] = Rows,
             binary_to_list(R)
    end.

set_sipuri(IMSAssociation)->
    Id = maps:get(user, IMSAssociation),
    PubId = maps:get(pubid, IMSAssociation),
    C = connect(),
    {ok, _} = epgsql:equery(C, "update srd_user set sipurl=$1, updated=current_timestamp where id=$2", [PubId,Id]),
    epgsql:close(C). 
       
set_sipuri_default(IMSAssociation)->
    Id = maps:get(user, IMSAssociation),
    C = connect(),
    {ok, _} = epgsql:equery(C, "update srd_user set sipurl=$1, updated=current_timestamp where id=$1", [Id]),
    epgsql:close(C).
    
get_group(Id) ->
    C = connect(),
    {ok, _, Rows} = epgsql:equery(C, "select trim(group_id) from srd_user where id= $1", [Id]),
    ok = epgsql:close(C),
    case Rows of
        [] -> undefined;
        _ -> [{R}] = Rows,
             binary_to_list(R)
    end.
    
set_pass(UserId, Pass)->
    C = connect(),
    {ok, _} = epgsql:equery(C, "update srd_user set password=$1, updated=current_timestamp where id=$2", [Pass,UserId]),
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
    
get_users(GrpId) ->
    C = connect(),
    {ok, _, Rows} = epgsql:equery(C, "select trim(id) from srd_user where group_id=$1", [GrpId]),
    ok = epgsql:close(C),
    Rows.
    
set_vmail(UserName, MailUser, MailPass) ->
    C = connect(),
    {ok, _} = epgsql:equery(C, "update srd_user set vmail_user=$1, vmail_password=$2, updated=current_timestamp where id=$3", [MailUser,MailPass,UserName]),
    epgsql:close(C).
        
delete_vmail(UserId)->
    C = connect(),
    {ok, _} = epgsql:equery(C, "update srd_user set vmail_user='NODATA', vmail_password='NODATA', updated=current_timestamp where id=$1", [UserId]),
    epgsql:close(C).
    
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
    {ok, _, Rows} = epgsql:equery(C, "select trim(phonecontext) from srd_user where id=$1", [UserId]),
    ok = epgsql:close(C),
    case Rows of
        [] -> undefined;
        _ -> [{R}] = Rows,
             binary_to_list(R)
    end.
    
set_phonecontext(UserId, PhoneContext)->
    C = connect(),
    {ok, _} = epgsql:equery(C, "update srd_user set phonecontext=$1, updated=current_timestamp where id=$2", [PhoneContext,UserId]),
    epgsql:close(C).
        
%%%===================================================================
%%% Internal functions
%%%===================================================================
connect() ->
    {ok, PGHost} = application:get_env(em, pg_host),
    {ok, C} = epgsql:connect(PGHost, "srd", "srd", [{database, "srd"},{timeout, 4000}]),
    C.

event_to_json({Id,User,Command}) ->
    ?INFO_MSG("Event to JSON: ~p~n", [Command]), 
	#{id=>Id, command=>Command, user=>User}.