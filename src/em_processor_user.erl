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
    em_ema_server:create_hss_subscriber(Event),
    {ok, _} = em_srd:add_user(Event).       
    
delete(#event{user=UserName}) ->
    [{CurrentPhone}] = em_srd:get_e164(UserName),
    CurrentPhone1 = binary_to_list(CurrentPhone),
    
    [{MailUser}] = em_srd:get_vmail_user(UserName),
    MailUser1 = binary_to_list(MailUser),
    
        case MailUser1 of
            "NODATA" ->
                ok;
            _ ->
                em_processor_vmail:delete_account(MailUser1)
                
        end,
    
        case CurrentPhone1 of
            "NODATA" ->
               em_ema_server:delete_hss_subscriber(UserName),
               {ok, _} = em_srd:delete_user(UserName);
           
            _ ->
                em_ema_server:delete_enum(CurrentPhone1),
                em_ema_server:delete_hss_subscriber(UserName),
                {ok, _} = em_srd:delete_user(UserName)
        end.


modify(Event=#event{current_type="trunk", phone=Phone,current_phone=CPhone}) ->
    ?INFO_MSG("Changing user type from trunk to user: ~n", []),
    delete(Event),
    create(Event),
    
    case CPhone of
        Phone ->
            em_srd:set_e164(Event),
            em_ema_server:create_hss_teluri(Event),
            em_ema_server:create_enum(Event);
       
        _ ->
            em_ema_server:delete_enum(CPhone)
    end,
    
    
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
    em_ema_server:delete_enum(X2),
    em_ema_server:delete_hss_teluri(Z1, X2),
    em_srd:delete_e164(Z1);
        
modify(Event=#event{user=Z1, pubid=_X1, current_pubid=X2, phone=nil, current_phone="NODATA"}) ->
    ?INFO_MSG("Update pubId )~n", []),
    em_ema_server:delete_hss_pubid(Z1, X2),
    em_ema_server:delete_hss_serviceprofile(Z1, X2),
    em_srd:set_sipuri(Event),
    em_ema_server:create_hss_serviceprofile(Event),
    em_ema_server:create_hss_pubid(Event);
       
modify(Event=#event{user=Z1, pubid=Z1, phone=_X1, current_pubid=Z1, current_phone="NODATA"}) ->
    ?INFO_MSG("Create phone on default pubid (no change to PubID))~n", []),
    em_srd:set_e164(Event),
    em_ema_server:create_hss_teluri(Event),
    em_ema_server:create_enum(Event);

modify(#event{user=Z1, pubid=Z1, phone=nil, current_pubid=Z1, current_phone=X2}) ->
    ?INFO_MSG("Delete phone on default pubid (no change to PubID))~n", []),
    em_ema_server:delete_enum(X2),
    em_ema_server:delete_hss_telur(Z1, X2),
    em_srd:delete_e164(Z1);
    
modify(Event=#event{user=Z1, pubid=Z1, phone=_X1, current_pubid=Z1, current_phone=X2}) ->
    ?INFO_MSG("Update phone on default pubid (no change to PubID))~n", []),
    em_ema_server:delete_enum(X2),
    em_ema_server:delete_hss_teluri(Z1, X2),
    em_srd:set_e164(Event),
    em_ema_server:create_hss_teluri(Event),
    em_ema_server:create_enum(Event);
        
modify(Event=#event{user=Z1, pubid=_X1, phone=_Y1, current_pubid=X2, current_phone="NODATA"}) ->
    ?INFO_MSG("Create phone on new pubId ~n", []),
    em_ema_server:delete_hss_pubid(Z1, X2),
    em_ema_server:delete_hss_serviceprofile(Z1, X2),
    em_srd:set_sipuri(Event),
    em_srd:set_e164(Event),
    em_ema_server:create_hss_serviceprofile(Event),
    em_ema_server:create_hss_pubid(Event),
    em_ema_server:create_hss_telur(Event),
    em_ema_server:create_enum(Event);

modify(Event=#event{user=Z1, pubid=_X1, phone=nil, current_pubid=X2, current_phone=X3}) ->
    ?INFO_MSG("Delete phone on new pubId ~n", []),
    em_ema_server:delete_enum(X3, X2),
    em_ema_server:delete_hss_teluri(Z1, X3),
    em_ema_server:delete_hss_pubid(Z1, X2),
    em_ema_server:delete_hss_serviceprofile(Z1, X2),
    em_srd:set_sipuri(Event),
    em_srd:delete_e164(Z1),
    em_ema_server:create_hss_serviceprofile(Event),
    em_ema_server:create_hss_pubid(Event);

modify(Event=#event{user=Z1, pubid=_X1, phone=_Y1, current_pubid=X2, current_phone=X3}) ->
    ?INFO_MSG("Update phone on new pubId ~n", []),
    em_ema_server:delete_enum(X3),
    em_ema_server:delete_hss_telur(Z1, X3),
    em_ema_server:delete_hss_pubid(Z1, X2),
    em_ema_server:delete_hss_serviceprofile(Z1, X2),
    em_srd:set_sipuri(Event),
    em_srd:set_e164(Event),
    em_ema_server:create_hss_serviceprofile(Event),
    em_ema_server:create_hss_pubid(Event),
    em_ema_server:create_hss_teluri(Event),
    em_ema_server:create_enum(Event);

modify(_) ->
    ?ERROR_MSG("Invalid Event record state: ~n", []).  
    
set_password(UserName, Pass) ->
    em_ema_server:update_hss_pass(UserName, Pass).
    %em_srd:set_pass(UserName, Pass).


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
    em_ema_server:update_hss_phonecontext(UserName, PhoneContext),
    em_srd:set_phonecontext(UserName, PhoneContext).
%modify(#event{user=X1, pubid=X2, phone=X3, current_pubid=X4, current_phone=X5}) ->
%    ?INFO_MSG("User: ~p", [X1]),
%    ?INFO_MSG("PubId: ~p", [X2]),
%    ?INFO_MSG("Phone: ~p", [X3]),
%    ?INFO_MSG("CurrentPubId: ~p", [X4]),
%    ?INFO_MSG("CurrentPhone: ~p", [X5]).     



