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

-module(em_voicemail).

-export([modify/2, modify_vm/2]).
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

modify(Id, Message) ->
    logger:debug("Start processing vmail"), 
    InsideCommand = em_utils:get_element_childs(Message),
    [U] = em_utils:get_elements(userId, InsideCommand),
    [G] = em_utils:get_elements(groupMailServerUserId, InsideCommand),
    [P] = em_utils:get_elements(groupMailServerPassword, InsideCommand),
    User = em_utils:get_element_text(U),
    MailUser = em_utils:get_element_text(G),
    MailPass = em_utils:get_element_text(P),
    logger:debug("Processing vmail:~p", [MailUser]), 
                    
    Attrs = #{
        user        => User,
        mailuser    => MailUser,
        mailpass    => MailPass,
        current_mailuser   => em_srd:get_vmail_user(User),
        current_mailpass   => em_srd:get_vmail_pass(User)
    },
    ok = process(modify_vm, Id, User, Attrs).
    
%%%===================================================================
%%% Internal functions
%%%===================================================================

% We are awaiting the result since all processing must be _strictly_ sequential.
process(F, Id, User, Attrs) ->
    {Pid, Ref} = spawn_monitor(?MODULE, F, [User, Attrs]),
    Reply = await_result(Pid, Ref, Id),
    Reply.

modify_vm(_User, Attrs) ->
    em_surgemail:modify(Attrs).
   

% Waiting for the process to finish.        
await_result(Pid, Ref, Id) ->
    receive
                
        {'DOWN', Ref, process, Pid, normal} ->
            logger:debug("Event processing successfull: ~p", [Id]),
            em_db:complete_event(Id),
            ok;

        {'DOWN', Ref, process, Pid, Reason} ->
            logger:error("Event processing failed: ~p, ~p", [Id, Reason]),
            fail_event(Id),
            error
            
    after 8000 ->
            logger:error("Event process timeout: ~p", [Id]),
            fail_event(Id),
            error
    end.      


fail_event(Id) ->
    em_db:fail_event(Id).