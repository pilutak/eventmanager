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
    X = em_db:get_event_status(),
    CountFailedEvents = list_to_integer(binary_to_list(X)),
    Json = jsx:encode([{<<"is_alive">>,true}, {<<"failed_events">>,CountFailedEvents}]),
    {ok, [], Json};


handle('GET', [<<"events">>], Req) ->
	AllEvents = elli_request:get_arg_decoded(<<"all">>, Req, <<"undefined">>),
	Response = em_db:get_events(AllEvents),
	%Response = #{'id' => 1, 'user' => <<"admin">>, command => <<"UserAdd">>, status => <<"completed">>},
    %Response1 = jsx:encode(Response),    
	ResponseBody = term_to_json(Response),
	{ok, [{<<"Content-type">>, <<"application/json">>}],
	ResponseBody};


handle('GET', [<<"event">>, Id], _Req) ->
    Id1 = binary_to_list(Id),
    Id2 = list_to_integer(Id1),
	[Response] = em_db:get_event(Id2),
	ResponseBody = term_to_json(Response),
	{ok, [{<<"Content-type">>, <<"application/json">>}],
	ResponseBody};

handle(_, _, _Req) ->
    {404, [], <<"Not Found">>}.

%% @doc: Handle request events, like request completed, exception
%% thrown, client timeout, etc. Must return 'ok'.
handle_event(_Event, _Data, _Args) ->
    ok.


%%%===================================================================
%%% Internal functions
%%%===================================================================

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