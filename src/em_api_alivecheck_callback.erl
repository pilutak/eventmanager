-module(em_api_alivecheck_callback.erl).
-export([handle/2, handle_event/3]).

-include_lib("elli/include/elli.hrl").
-behaviour(elli_handler).

-record(em_srd, {user, pass, pubid, phone, group, vmuser, vmpass, type}).

handle(Req, _Args) ->
    %% Delegate to our handler function
    handle(Req#req.method, elli_request:path(Req), Req).

%% curl -X GET -H "Accept: application/json" http://localhost:3000/user/{UserName}
handle('GET', [<<"users">>, Id], _Req) ->
	Data = em_srd:get_user(binary_to_list(Id)),
    Response = [user_to_json(P) || P <- Data],
    
	ResponseBody = term_to_json(Response),
	{ok, [{<<"Content-type">>, <<"application/json">>}],
	ResponseBody};

handle(_, _, _Req) ->
    {404, [], <<"Not Found">>}.

%% @doc: Handle request events, like request completed, exception
%% thrown, client timeout, etc. Must return 'ok'.
handle_event(_Event, _Data, _Args) ->
    ok.
    
term_to_json(Term) ->
    jsx:encode(fixup(Term)).
	%jsx:encode(Term).
	
%% Ground types
fixup(Term) when is_number(Term) -> Term;
fixup(Term) when is_atom(Term) -> Term;
fixup(Term) when is_binary(Term) -> Term;
%% Compound types
fixup(Term) when is_list(Term) ->
    [fixup(T) || T <- Term];
fixup(Term) when is_map(Term) ->
    KVs = maps:to_list(Term),
    maps:from_list([{fixup_key(K), fixup(V)} || {K, V} <- KVs]);
fixup(Term) ->
    %% Every other term is transformed into a binary value
    iolist_to_binary(
      io_lib:format("~p", [Term])).

fixup_key(Term) ->
    case fixup(Term) of
        T when is_binary(T) ->
            T;
        T ->
            iolist_to_binary(io_lib:format("~p", [T]))
    end.
    

user_to_json(#em_srd{user = Id, phone = Phone}) ->
	#{user => list_to_binary(Id), phone => list_to_binary(Phone)}.