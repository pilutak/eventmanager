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

-module(em_sup).
-export([start_link/0, init/1]).
-behaviour(supervisor).

-define(SUPERVISOR, ?MODULE).

 
start_link() ->
    supervisor:start_link({local, ?SUPERVISOR}, ?MODULE, []).
 
init([]) ->
    EMOCIRSUP =
        {em_reader_sup,
         {em_reader_sup, start_link, []},
         permanent,
         infinity,
         supervisor,
         [em_reader_sup]},
    EMA =
        {em_ema,
         {em_ema, start_link, []},
         permanent,
         5000,
         worker,
        [em_ema]},

    SURGEMAIL =
        {em_surgemail,
         {em_surgemail, start_link, []},
         permanent,
         5000,
         worker,
        [em_surgemail]},
         
         
    ElliOpts = [{callback, em_api_callback}, {port, 8080}],
    ElliSpec = {
        fancy_http,
        {elli, start_link, [ElliOpts]},
        permanent,
        5000,
        worker,
        [elli]},
    
    {ok,{{one_for_one,10,1},
	 [ElliSpec, EMA, SURGEMAIL, EMOCIRSUP]}}.    
