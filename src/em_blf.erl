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

-module(em_blf).

-export([modify/2, modify_blf/3]).



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
    logger:debug("Start processing BLF"), 
    User = get_user(Message),
                    
    Attrs = #{
        user        => User,
        uri         => get_uri(Message),
        type        => "virtual",
        irs         => "0",
        group       => em_srd:get_group(User),
        phonecontext=> "tg.gl" 

    },

    process(modify_blf, Id, User, Attrs).
    
%%%===================================================================
%%% Internal functions
%%%===================================================================

% We are awaiting the result since all processing must be _strictly_ sequential.
process(F, Id, User, Attrs) ->
    {ok, C} = open_ema_session(),    
    {Pid, Ref} = spawn_monitor(?MODULE, F, [C, User, Attrs]),

    % Awaiting result, processing must be sequential.
    Reply = await_result(Pid, Ref, Id),
    em_ema:close(C),
    Reply.


modify_blf(C, User, Attrs) ->
    logger:debug("BLF ATTRS: ~p", [Attrs]),    
    URI = em_srd:get_blf_user(User),
    NewURI = maps:get(uri, Attrs),
    case plan_blf(User, URI, NewURI) of
        ignore -> 
            ok;
        create -> 
            em_srd:set_blf_user(User, NewURI),
            ok = em_hss_association:create(NewURI, Attrs, C); 
        modify ->
            em_hss_association:delete(URI, C),
            ok = em_hss_association:create(NewURI, Attrs, C),
            em_srd:set_blf_user(User, NewURI);
        delete ->
            em_srd:delete_blf(User),
            em_hss_association:delete(URI, C)
    end.


    

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

get_uri(Message) ->
    InsideCommand = em_utils:get_element_childs(Message),
    [URI] = em_utils:get_elements(listURI, InsideCommand),
    fix_nil_uri(URI).

get_user(Message) ->
    InsideCommand = em_utils:get_element_childs(Message),
    [U] = em_utils:get_elements(userId, InsideCommand),
    em_utils:get_element_text(U).

plan_blf(_User, _URI, _URI) ->
    logger:debug("BLF ignore because new and old URI is identical or undefined"),
    ignore;
plan_blf(_NewURI, undefined, _NewURI) ->
    logger:debug("BLF ignore because new URI matches userID"),
    ignore;    
plan_blf(_User, _URI, undefined) ->
    logger:debug("BLF delete because new URI is undefined"),
    delete;
plan_blf(_NewURI, _URI, _NewURI) ->
    logger:debug("BLF delete because new URI matches userID and URI exists"),
    delete;    
plan_blf(_User, undefined, _NewURI) ->
    logger:debug("BLF create"),
    create;
plan_blf(_User, _URI, _NewURI) ->
    logger:debug("BLF modify"),
    modify.


fix_nil_uri(Element) ->
    IsNil =  em_utils:get_element_attributes('xsi:nil', Element) =:= "true",
    Text = em_utils:get_element_text(Element),    
    
    case {Text, IsNil} of
        {_, false} -> Text;
        {_, true} -> undefined
    end.

fail_event(Id) ->
    em_db:fail_event(Id).


open_ema_session() ->
    PriHost = econfig:get_value(em, "ema", "primary_host"),
    SecHost = econfig:get_value(em, "ema", "secondary_host"),
    User = econfig:get_value(em, "ema", "username"),
    Pass = econfig:get_value(em, "ema", "password"),
        
    case em_ema:open(PriHost, User, Pass) of
        {ok, C} ->
            logger:debug("EMA session to ~p established", [PriHost]),
            {ok, C};
        _Other ->
            logger:debug("Attempting EMA session to ~p", [SecHost]),
            em_ema:open(SecHost, User, Pass)
    end.
