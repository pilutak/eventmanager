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

-module(em_processor_user).

-export([create/1, delete/1, modify/1, set_password/2, set_phonecontext/3]).
-include("../include/em.hrl").

%%%===================================================================
%%% API
%%%===================================================================
create(Event) ->
    State=#state{session=em_ema_session:open()},
    em_hss:create({subscriber, Event}, State),
    {ok, _} = em_srd:add_user(Event),
    em_ema_session:close(State).
        
    
delete(#event{user=UserName}) ->
    [{CurrentPubId}] = em_srd:get_sipuri(UserName),
    [{CurrentPhone}] = em_srd:get_e164(UserName),

    CurrentPubId1 = binary_to_list(CurrentPubId),
    CurrentPhone1 = binary_to_list(CurrentPhone),
    
    [{MailUser}] = em_srd:get_vmail_user(UserName),
    MailUser1 = binary_to_list(MailUser),
    
        case MailUser1 of
            "NODATA" ->
                ok;
            _ ->
                em_processor_vmail:delete_account(MailUser1)
                
        end,
    

    State=#state{session=em_ema_session:open()},
    
        case CurrentPhone1 of
            "NODATA" ->
               em_hss:delete({subscriber, UserName}, State),
               {ok, _} = em_srd:delete_user(UserName);
           
            _ ->
                em_hss:delete({enum, CurrentPhone1, CurrentPubId1}, State),
                em_hss:delete({subscriber, UserName}, State),
                {ok, _} = em_srd:delete_user(UserName)
        end,
    em_ema_session:close(State). 


modify(Event=#event{current_type=trunk}) ->
    ?INFO_MSG("Changing user type from trunk to user: ~n", []),
    delete(Event),
    create(Event),
    modify(Event#event{current_type=user});

modify(#event{pubid=undefined, phone=undefined}) ->
    ?INFO_MSG("Ignoring (all undefined): ~n", []); 
    
modify(#event{user=X1, pubid=X1, phone=nil, current_pubid=X1, current_phone="NODATA"}) ->
    ?INFO_MSG("Ignoring ~n", []);
    
modify(#event{user=Z1, pubid=Z1, phone=nil, current_pubid=Z1, current_phone="NODATA"}) ->
    ?INFO_MSG("Ignoring, all default)~n", []);

modify(#event{user=_Z1, pubid=X1, phone=nil, current_pubid=X1, current_phone="NODATA"}) ->
    ?INFO_MSG("Ignoring no change to pubid, no phone()~n", []);  

modify(#event{user=Z1, pubid=Z1, phone=undefined, current_pubid=Z1, current_phone="NODATA"}) ->
    ?INFO_MSG("Ignoring))~n", []);

modify(#event{user=Z1, pubid=Z1, phone=X1, current_pubid=Z1, current_phone=X1}) ->
    ?INFO_MSG("Ignoring no change to phone()~n", []);  

modify(#event{user=Z1, pubid=X1, current_pubid=X1, phone=nil, current_phone=X2}) ->
    ?INFO_MSG("Delete phone (no pubid change))~n", []),
    State=#state{session=em_ema_session:open()},
    em_hss:delete({enum, X2, X1}, State),
    em_hss:delete({teluri, Z1, X2}, State),
    em_srd:delete_e164(Z1),
    em_ema_session:close(State);  
    
modify(Event=#event{user=Z1, pubid=_X1, current_pubid=X2, phone=nil, current_phone="NODATA"}) ->
    ?INFO_MSG("Update pubId )~n", []),
    State=#state{session=em_ema_session:open()},
    em_hss:delete({pubid, Z1, X2}, State),
    em_hss:delete({serviceprofile, Z1, X2}, State),
    em_srd:set_sipuri(Event),
    em_hss:create({serviceprofile, Event}, State),
    em_hss:create({pubid, Event}, State),
    em_ema_session:close(State);  
       
modify(Event=#event{user=Z1, pubid=Z1, phone=_X1, current_pubid=Z1, current_phone="NODATA"}) ->
    ?INFO_MSG("Create phone on default pubid (no change to PubID))~n", []),
    State=#state{session=em_ema_session:open()},
    em_srd:set_e164(Event),
    em_hss:create({teluri, Event}, State),
    em_hss:create({enum, Event}, State),
    em_ema_session:close(State); 

modify(#event{user=Z1, pubid=Z1, phone=nil, current_pubid=Z1, current_phone=X2}) ->
    ?INFO_MSG("Delete phone on default pubid (no change to PubID))~n", []),
    State=#state{session=em_ema_session:open()},
    em_hss:delete({enum, X2, Z1}, State),
    em_hss:delete({teluri, Z1, X2}, State),
    em_srd:delete_e164(Z1),
    em_ema_session:close(State);  

modify(Event=#event{user=Z1, pubid=Z1, phone=_X1, current_pubid=Z1, current_phone=X2}) ->
    ?INFO_MSG("Update phone on default pubid (no change to PubID))~n", []),
    State=#state{session=em_ema_session:open()},
    em_hss:delete({enum, X2, Z1}, State),
    em_hss:delete({teluri, Z1, X2}, State),
    em_srd:set_e164(Event),
    em_hss:create({teluri, Event}, State),
    em_hss:create({enum, Event}, State),
    em_ema_session:close(State);  
    
modify(Event=#event{user=Z1, pubid=_X1, phone=_Y1, current_pubid=X2, current_phone="NODATA"}) ->
    ?INFO_MSG("Create phone on new pubId ~n", []),
    State=#state{session=em_ema_session:open()},
    em_hss:delete({pubid, Z1, X2}, State),
    em_hss:delete({serviceprofile, Z1, X2}, State),
    em_srd:set_sipuri(Event),
    em_srd:set_e164(Event),
    em_hss:create({serviceprofile, Event}, State),
    em_hss:create({pubid, Event}, State),
    em_hss:create({teluri, Event}, State),
    em_hss:create({enum, Event}, State),
    em_ema_session:close(State);     


modify(Event=#event{user=Z1, pubid=_X1, phone=nil, current_pubid=X2, current_phone=X3}) ->
    ?INFO_MSG("Delete phone on new pubId ~n", []),
    State=#state{session=em_ema_session:open()},
    em_hss:delete({enum, X3, X2}, State),
    em_hss:delete({teluri, Z1, X3}, State),
    em_hss:delete({pubid, Z1, X2}, State),
    em_hss:delete({serviceprofile, Z1, X2}, State),
    em_srd:set_sipuri(Event),
    em_srd:delete_e164(Z1),
    em_hss:create({serviceprofile, Event}, State),
    em_hss:create({pubid, Event}, State),
    em_ema_session:close(State);


modify(Event=#event{user=Z1, pubid=_X1, phone=_Y1, current_pubid=X2, current_phone=X3}) ->
    ?INFO_MSG("Update phone on new pubId ~n", []),
    State=#state{session=em_ema_session:open()},
    em_hss:delete({enum, X3, X2}, State),
    em_hss:delete({teluri, Z1, X3}, State),
    em_hss:delete({pubid, Z1, X2}, State),
    em_hss:delete({serviceprofile, Z1, X2}, State),
    em_srd:set_sipuri(Event),
    em_srd:set_e164(Event),
    em_hss:create({serviceprofile, Event}, State),
    em_hss:create({pubid, Event}, State),
    em_hss:create({teluri, Event}, State),
    em_hss:create({enum, Event}, State),
    em_ema_session:close(State).

set_password(UserName, Pass) ->
    State=#state{session=em_ema_session:open()},
    em_hss:update({pass, UserName, Pass}, State),
    em_srd:set_pass(UserName, Pass),
    em_ema_session:close(State).



set_phonecontext(_UserName, PhoneContext, PhoneContext) ->
    ignore;
set_phonecontext(_UserName, undefined, "NODATA") ->
    ignore;
    
%set_phonecontext(UserName, undefined, _CurrentPhoneContext) ->
%    State=#state{session=em_ema_session:open()},
%    em_hss:update({phonecontext, UserName, "tg.gl"}, State),
%    em_srd:set_phonecontext(UserName, "tg.gl"),
%    em_ema_session:close(State);
            
set_phonecontext(UserName, PhoneContext, _CurrentPhoneContext) ->
    State=#state{session=em_ema_session:open()},
    em_hss:update({phonecontext, UserName, PhoneContext}, State),
    em_srd:set_phonecontext(UserName, PhoneContext),
    em_ema_session:close(State).

%modify(#event{user=X1, pubid=X2, phone=X3, current_pubid=X4, current_phone=X5}) ->
%    ?INFO_MSG("User: ~p", [X1]),
%    ?INFO_MSG("PubId: ~p", [X2]),
%    ?INFO_MSG("Phone: ~p", [X3]),
%    ?INFO_MSG("CurrentPubId: ~p", [X4]),
%    ?INFO_MSG("CurrentPhone: ~p", [X5]).     



