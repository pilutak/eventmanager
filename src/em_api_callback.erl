-module(em_api_callback).
-export([handle/2, handle_event/3]).

-include_lib("elli/include/elli.hrl").
-behaviour(elli_handler).

handle(Req, _Args) ->
    %% Delegate to our handler function
    handle(Req#req.method, elli_request:path(Req), Req).

handle('GET',[<<"alivecheck">>], _Req) ->
    %% Reply with a normal response. 'ok' can be used instead of '200'
    %% to signal success.
    Json = jsx:encode([{<<"is_alive">>,true}]),
    {ok, [], Json};


handle('GET', [<<"events">>], _Req) ->
	
	Response = em_db:get_events(),
	%Response = #{'id' => 1, 'user' => <<"admin">>, command => <<"UserAdd">>, status => <<"completed">>},
    %Response1 = jsx:encode(Response),    
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