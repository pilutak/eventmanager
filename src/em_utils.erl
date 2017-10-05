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

%% API
-export([scan/3,get_type_message/1,get_cdata/1,get_command_type/1,get_elements/2,get_element_attributes/2]).
-export([get_element_childs/1,get_element_name/1,get_element_text/1]).
-export([log/1,log/2]).

scan({xmlDocument,Content}=Data,Function,Record)->
  lists:foldl(fun(X,Acc)->Function(X,Acc)end,Function(Data,Record),Content);

scan({xmlElement,_Name,_,_,_,_,_,Attributes,Content,_,_,_}=Data,Function,Record)->
    R=lists:foldl(fun(X,Acc)->scan(X,Function,Acc) end,Function(Data,Record),Attributes), %% Scanning on Attributes
    lists:foldl(fun(X,Acc)->scan(X,Function,Acc) end,R,Content); %% Scanning on Content

scan(Data,Function,Record)->
  Function(Data,Record).


get_cdata(Parsed_Data)->
  scan(Parsed_Data,fun({xmlText,_,_,_,Value,cdata},_)->Value; (_,Acc)->Acc end,[]).

get_type_message(Parsed_Data)->
  case Parsed_Data of
    {xmlElement,'BroadsoftOCIReportingDocument',_,_,_,_,_,_,_,_,_,_} -> 'BroadsoftOCIReportingDocument';
    {xmlElement,'BroadsoftDocument',_,_,_,_,_,_,_,_,_,_} -> 'BroadsoftDocument';
    {xmlElement,_,_,_,_,_,_,_,_,_,_,_} -> not_implemented;
    _Other -> undefined
  end.

get_command_type(Parsed_Data)->
  scan(Parsed_Data,fun({xmlAttribute,'xsi:type',_,_,_,_,_,_,Value,_},_)->Value; (_,Acc)->Acc end,[]).


get_element_name({xmlElement,Name,_,_,_,_,_,_,_,_,_,_})->Name;
get_element_name(_)->undefined.

get_element_text({xmlElement,_,_,_,_,_,_,_,[{xmlText,_,_,_,Value,text}],_,_,_})->Value;
get_element_text(_)->undefined.

get_elements(_,[])->[];
get_elements(Name,[{xmlElement,Name,Name,Nsinfo,Namespace,Parents,Pos,
                    Attributes,Content,Lenguage,Xmlbase,ElementDef}|Rest])->
                  [{xmlElement,Name,Name,Nsinfo,Namespace,Parents,Pos,
                    Attributes,Content,Lenguage,Xmlbase,ElementDef}|get_elements(Name,Rest)];
get_elements(Name,[_|Rest])->get_elements(Name,Rest).


get_element_attributes(Name,{xmlElement,_,_,_,_,_,_,Attribute_list,_,_,_,_})->get_element_attributes(Name,Attribute_list);
get_element_attributes(Name,[{xmlAttribute,Name,_,_,_,_,_,_,Value,_}|_Rest])->Value;
get_element_attributes(Name,[_|Rest])->get_element_attributes(Name,Rest);
get_element_attributes(_,_)->undefined.


get_element_childs(XmlElement)->{xmlElement,_,_,_,_,_,_,_,Childs,_,_,_}=XmlElement,Childs.

log(Term)->
    {ok,S} = file:open("em2.log",[append]),
    io:format(S,"~p ~n",[Term]),
    file:close(S).

log(Message,List)->
    {ok,S} = file:open("em2.log",[append]),
    io:format(S,Message,List),
    file:close(S).


