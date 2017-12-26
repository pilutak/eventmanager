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

-module(em_reader_sup).
-behaviour(supervisor).

-export([start_link/0, open/1]).

%% supervisor.
-export([init/1]).

-define(SUPERVISOR, ?MODULE).

%%%===================================================================
%%% API
%%%===================================================================

-spec start_link() -> {ok, pid()}.
start_link() ->
    supervisor:start_link({local, ?SUPERVISOR}, ?MODULE, []).

open(Host) ->
    {ok, Pid} = supervisor:start_child(?MODULE, [Host]),
    register(list_to_atom(Host), Pid).

%% supervisor.

init([]) ->
    Procs = [{em_reader, {em_reader, start_link, []},
	      permanent, 5000, worker, [em_reader]}],
    {ok, {{simple_one_for_one, 10, 10}, Procs}}.
