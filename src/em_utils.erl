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

-module(em_utils).

-include_lib("xmerl/include/xmerl.hrl").

%% API
-export([
    scan/3,
    get_type_message/1,
    get_cdata/1,
    get_command_type/1,
    get_elements/2,
    get_element_attributes/2,
    get_element_childs/1,
    get_element_name/1,
    get_element_text/1,
    log/1,
    log/2,
    randchar/1,
    md5_hex/1
    ]).
    
%%%===================================================================
%%% API
%%%===================================================================

scan({xmlDocument,Content} = Data, Function, Record) ->
    lists:foldl(fun(X,Acc) -> Function(X, Acc) end, Function(Data, Record), Content);

scan({xmlElement,_Name,_,_,_,_,_,Attributes,Content,_,_,_} = Data, Function, Record) ->
    R = lists:foldl(fun(X, Acc) -> scan(X, Function, Acc) end, Function(Data, Record), Attributes), %% Scanning on Attributes
    lists:foldl(fun(X,Acc) -> scan(X, Function, Acc) end, R , Content); %% Scanning on Content

scan(Data, Function, Record) ->
    Function(Data, Record).

get_cdata(ParsedData) ->
    scan(ParsedData, fun({xmlText,_,_,_,Value,cdata},_) -> Value; (_,Acc) -> Acc end,[]).

get_type_message(ParsedData) ->
    case ParsedData of
        {xmlElement,'BroadsoftOCIReportingDocument',_,_,_,_,_,_,_,_,_,_} -> 'BroadsoftOCIReportingDocument';
        {xmlElement,'BroadsoftDocument',_,_,_,_,_,_,_,_,_,_} -> 'BroadsoftDocument';
        {xmlElement,_,_,_,_,_,_,_,_,_,_,_} -> not_implemented;
        _Other -> undefined
    end.

get_command_type(ParsedData) ->
    scan(ParsedData,fun({xmlAttribute,'xsi:type',_,_,_,_,_,_,Value,_},_) -> Value; (_, Acc) -> Acc end,[]).

get_element_name({xmlElement,Name,_,_,_,_,_,_,_,_,_,_}) -> Name;
get_element_name(_) -> undefined.

get_element_text({xmlElement,_,_,_,_,_,_,_,[{xmlText,_,_,_,Value,text}],_,_,_}) -> Value;
get_element_text(_) -> undefined.


get_elements(_,undefined) -> [undefined];
get_elements(Name, List) ->
    F = fun
        (#xmlElement{name = ItemName}=E,[undefined]) when ItemName == Name -> [E];
        (#xmlElement{name = ItemName}=E,Acc) when ItemName == Name -> [E|Acc];
        (_,Acc) -> Acc
    end,
    lists:foldl(F, [undefined], List).

get_element_attributes(Name,{xmlElement,_,_,_,_,_,_,AttributeList,_,_,_,_}) -> get_element_attributes(Name, AttributeList);
get_element_attributes(Name,[{xmlAttribute,Name,_,_,_,_,_,_,Value,_}|_Rest]) -> Value;
get_element_attributes(Name,[_|Rest]) -> get_element_attributes(Name, Rest);
get_element_attributes(_,_) -> undefined.

get_element_childs(undefined) -> undefined; 
get_element_childs([]) -> undefined; 
get_element_childs(#xmlElement{content = []}) -> undefined;
get_element_childs(#xmlElement{content = Childs}) -> Childs.

% We use this to create a temporarily SIP password. The password is later
% overwritten by an seperate event (not for virtual users, the password remains).
randchar(N) ->
   randchar(N, []).
   
randchar(0, Acc) ->
   Acc;
randchar(N, Acc) ->
   randchar(N - 1, [rand:uniform(26) + 96 | Acc]). 
   
md5_hex(S) ->
    <<X:128/integer>> = crypto:hash(md5, S),
    lists:flatten(io_lib:format("~32.16.0b", [X])).  
%%%===================================================================
%%% Internal Functions
%%%===================================================================