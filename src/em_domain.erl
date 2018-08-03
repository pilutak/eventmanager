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

-module(em_domain).

-export([create/2, delete/2, create_domain/1, delete_domain/1]).
-include("../include/em.hrl").


%%%===================================================================
%%% API
%%%===================================================================
% Attrs can hold following values:
%
% pubid: bob@home.net (default is USER)
% sprofile: bob@home.net (Default is PUBID)
% irs: "0" | "1" (default is "0")
% isdefault "true" | "false" (default is "false")
% phone: 666666 
% group: VK11111118
% type: user | virtual | pilot | trunk
% csprofile: user | virtual | trunk_pilot | trunk_ddi (default is TYPE)

create(Id, Message) ->
    InsideCommand = em_utils:get_element_childs(Message),
    [D] = em_utils:get_elements(domain, InsideCommand),
    Domain = em_utils:get_element_text(D),
    process(create_domain, Id, Domain).


delete(Id, Message) ->
    InsideCommand = em_utils:get_element_childs(Message),
    [D] = em_utils:get_elements(domain, InsideCommand),
    Domain = em_utils:get_element_text(D),
    process(delete_domain, Id, Domain).

%%%===================================================================
%%% Internal functions
%%%===================================================================

% We are awaiting the result since all processing must be _strictly_ sequential.
process(F, Id, Domain) ->
    {Pid, Ref} = spawn_monitor(?MODULE, F, [Domain]),
    Reply = await_result(Pid, Ref, Id),
    Reply.

create_domain(Domain) ->
    em_surgemail:create_domain(Domain).    
    
delete_domain(Domain) ->
    em_surgemail:delete_domain(Domain).
   

% Waiting for the process to finish.        
await_result(Pid, Ref, Id) ->
    receive
                
        {'DOWN', Ref, process, Pid, normal} ->
            logger:notice("Event processing successfull: ~p, ~p", [Ref, Pid]),
            logger:notice("Completing event: ~p", [Id]),
            em_db:complete_event(Id),
            ok;

        {'DOWN', Ref, process, Pid, Reason} ->
            logger:debug("Event processing failed: ~p, ~p, ~p", [Ref, Pid, Reason]),
            fail_event(Id),
            error
            
    after 8000 ->
            logger:error("Event process timeout"),
            error
    end.      


fail_event(Id) ->
    em_db:fail_event(Id).