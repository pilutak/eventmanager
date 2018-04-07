%%%-------------------------------------------------------------------
%%% @author Admin
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 05. abr 2018 03:30
%%%-------------------------------------------------------------------
-module(em_auth).
-author("Admin").

%% API
-export([add_session_id/2]).
-include_lib("xmerl/include/xmerl.hrl").


add_session_id(SessionId,Envelope)->
  {Item,_} = xmerl_scan:string(lists:flatten(Envelope)),
  Result = explore(Item,fun(X)-> check(X,SessionId) end ),
  Xml=xmerl:export_simple([Result],xmerl_xml),
  [S]=io_lib:format("~s",[lists:flatten(Xml)]),
  S2 = string:replace(S,[10],[13,10],all),
  [R]=io_lib:format("~s",[S2]),
  lists:flatten(io_lib:format("~s",[R])).


explore(#xmlElement{content = Content} =Item,Function)->
  NewContent = lists:map(Function,Content),
  Item#xmlElement{content = NewContent}.

check(#xmlElement{name = 'cai3:SessionId', pos = Position,parents = Parents} = Item,SessionId)->
  Item#xmlElement{content = [
    #xmlText{
      parents = [{'cai3:SessionId',Position}|Parents],
      pos = 1,
      type = text,
      language = [],
      value = SessionId
    }
  ]};

check(#xmlElement{} = Item,SessionId)->explore(Item,fun(X)-> check(X,SessionId) end );
check(E,_)->E.