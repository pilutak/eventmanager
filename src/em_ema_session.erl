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

-module(em_ema_session).

-export([open/0, close/1]).
-include("../include/em.hrl").

%%%===================================================================
%%% API
%%%===================================================================
open() ->
    {ok, Session} = em_hss:login(?EMA_USER, ?EMA_PASS),
    ?INFO_MSG("EMA session created: ~p", [Session]),
    Session.
    
close(#state{session=Session}) ->
    em_hss:logout(Session),
    ?INFO_MSG("EMA session closed: ~p", [Session]).    

%%%===================================================================
%%% Internal functions
%%%===================================================================
