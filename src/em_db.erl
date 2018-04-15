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
-include("../include/em.hrl").
 
-export([
    insert_event/3,
    insert_white_event/3,
    get_events/1,
    complete_event/1,
    set_white_event/1,
    fail_event/1
]).


%%%===================================================================
%%% API
%%%===================================================================

insert_event(UserId, Command, Event) ->    
    C = connect(),
    {ok, _, _, Rows} = epgsql:equery(C, "insert into em_event (user_id, command, event, status, inserted) values ($1,$2,$3,$4, current_timestamp) returning id", [UserId, Command, Event, "pending"]),
    epgsql:close(C),
    case Rows of
        [] -> undefined;
        _ -> [{R}] = Rows,
             R
             %binary_to_list(R)
    end.

insert_white_event(UserId, Command, Event) ->    
    C = connect(),
    {ok, _, _, Rows} = epgsql:equery(C, "insert into em_event (user_id, command, event, status, inserted) values ($1,$2,$3,$4, current_timestamp) returning id", [UserId, Command, Event, "ignored"]),
    epgsql:close(C),
    case Rows of
        [] -> undefined;
        _ -> [{R}] = Rows,
             R
             %binary_to_list(R)
    end.

get_events(<<"undefined">>) ->
    C = connect(),
    {ok, _, Rows} = epgsql:equery(C, "select id, user_id, command, status, extract(epoch from inserted) as datetime from em_event WHERE inserted > current_date - integer '7' ORDER BY inserted ASC", []),
    ok = epgsql:close(C),    
    case Rows of
        [] -> [];
        _ -> Rows,
            %?INFO_MSG("Rows: ~p~n", [Rows]), 
            [event_to_json(P) || P <- Rows]
    end;


get_events(Status) ->
    C = connect(),
    {ok, _, Rows} = epgsql:equery(C, "select id, user_id, command, status, extract(epoch from inserted) as datetime from em_event WHERE status = $1 and inserted > current_date - integer '7' ORDER BY inserted ASC", [Status]),
    ok = epgsql:close(C),    
    case Rows of
        [] -> [];
        _ -> Rows,
            %?INFO_MSG("Rows: ~p~n", [Rows]), 
            [event_to_json(P) || P <- Rows]
    end.


complete_event(Id) ->
    C = connect(),
    {ok, _} = epgsql:equery(C, "update em_event set status=$1 where id=$2", ["completed",Id]),
    epgsql:close(C).

set_white_event(Id) ->
    C = connect(),
    {ok, _} = epgsql:equery(C, "update em_event set status=$1 where id=$2", ["ignored",Id]),
    epgsql:close(C).

fail_event(Id) ->
    C = connect(),
    {ok, _} = epgsql:equery(C, "update em_event set status=$1 where id=$2", ["failed",Id]),
    epgsql:close(C).


%%%===================================================================
%%% Internal functions
%%%===================================================================
connect() ->
    {ok, Args} = application:get_env(em, em_db),
    Hostname = proplists:get_value(hostname, Args),
    Database = proplists:get_value(database, Args),
    Username = proplists:get_value(username, Args),
    Password = proplists:get_value(password, Args),

    {ok, C} = epgsql:connect(Hostname, Username, Password, [{database, Database},{timeout, 4000}]),
    C.

event_to_json({Id, User, Command, Status, Inserted}) ->
    %?INFO_MSG("Event to JSON: ~p~n", [Command]), 
	#{id=>Id, event=>Command, user=>User, status=>Status, timestamp=>Inserted}.