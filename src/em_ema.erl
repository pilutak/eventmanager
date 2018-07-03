%%%-------------------------------------------------------------------
%%% @Author Thomas Elsgaard <thomas.elsgaard@timezone4.com>
%%% @copyright (C) 2018, Thomas Elsgaard
%%% @doc
%%%
%%% @end
%%% Created : 15 Jun 2018 by Thomas Elsgaard <thomas.elsgaard@timezone4.com>
%%%-------------------------------------------------------------------
-module(em_ema).

%% API
-export([open/3,send/2,close/1]).

-type host() :: inet:ip_address() | inet:hostname().
-type conn() :: pid().
-type conn_err() :: em_ema_server:conn_err().

%%%===================================================================
%%% API
%%%===================================================================
-spec open(host(), string(), string()) -> {ok, Conn :: conn()} | {error, Reason :: conn_err()}.
open(Host, Username, Password) ->
    {ok, C} = em_ema_server:start_link(),
    open(C, Host, Username, Password).

-spec open(conn(), host(), string(), string()) -> {ok, Conn :: conn()} | {error, Reason :: conn_err()}.
open(C, Host, Username, Password) ->
    case em_ema_server:sync_command(
             C, start_session, {Host, Username, Password}) of
          {ok, _} -> 
              {ok, C};
           Error -> Error
      end.
      
-spec close(conn()) -> ok.
close(C) ->
    em_ema_server:close_session(C).

-spec send(conn(), term()) -> {ok, Result :: any()} | {error, Reason :: any()}.
send(Connection, MO) ->
    em_ema_server:sync_command(Connection, command, MO).

%%%===================================================================
%%% Internal functions
%%%===================================================================

