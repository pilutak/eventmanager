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

-module(em_processor_trunk).

-export([create/1, delete/1, modify/1]).
-include("../include/em.hrl").

%%%===================================================================
%%% API
%%%===================================================================
create(Event) ->
    ?INFO_MSG("Creating user: ~n", []),
    em_ema_server:create_hss_virtual_subscriber(Event),
    {ok, _} = em_srd:add_user(Event).     
    
delete(#event{user=UserName}) ->
    ?INFO_MSG("Deleting user: ~n", []),
    [{CurrentPhone}] = em_srd:get_e164(UserName),

    CurrentPhone1 = binary_to_list(CurrentPhone),
    
        case CurrentPhone1 of
            "NODATA" ->
               em_ema_server:delete_hss_subscriber(UserName),
               {ok, _} = em_srd:delete_user(UserName);
           
            _ ->
                em_ema_server:delete_enum(CurrentPhone1),
                em_ema_server:delete_hss_subscriber(UserName),
                {ok, _} = em_srd:delete_user(UserName)
        end.

    
modify(Event=#event{current_type="user"}) ->
    ?INFO_MSG("Changing user type to trunk: ~n", []),
    delete(Event),
    create(Event),
    
    %TODO: We must also re-create phone before performing modify
    modify(Event#event{current_type=trunk});
    
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
    em_ema_server:delete_hss_teluri(Z1, X2),
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
    em_ema_server:create_hss_teluri(Event),
    em_ema_server:create_enum(Event);

modify(Event=#event{user=Z1, pubid=_X1, phone=nil, current_pubid=X2, current_phone=X3}) ->
    ?INFO_MSG("Delete phone on new pubId ~n", []),
    em_ema_server:delete_enum(X3),
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
    em_ema_server:delete_hss_teluri(Z1, X3),
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

