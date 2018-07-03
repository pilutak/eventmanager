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

% used for migration
handle('GET', [<<"user">>, Id], _Req) ->
    Id1 = binary_to_list(Id),
    ResponseBody = em_srd:get_pass(Id1),
	%[Response] = em_srd:get_pass(Id1),
	%ResponseBody = term_to_json(Response),
    
    case em_srd:get_pass(Id1) of
        undefined -> {404, [], <<"Not Found">>};
        _-> {ok, [{<<"Content-type">>, <<"application/json">>}], ResponseBody}
        
    end;

% used for migration
handle('POST', [<<"users">>], Req) ->
	%Name = elli_request:body(Req),
	%io:format(Name),
    %% Fetch a POST argument from the POST body.
    %Name = elli_request:post_arg(<<"name">>, Req, <<"undefined">>),
    % Fetch and decode
    io:format("USERS CALLED"),
    Type = elli_request:post_arg_decoded(<<"type">>, Req, <<"undefined">>),
    Group = elli_request:post_arg_decoded(<<"group">>, Req, <<"undefined">>),
    Id = elli_request:post_arg_decoded(<<"id">>, Req, <<"undefined">>),
    Phone = elli_request:post_arg_decoded(<<"phone">>, Req, <<"undefined">>),
    City = elli_request:post_arg_decoded(<<"city">>, Req, <<"undefined">>),
        
    create_user(Type, Group, Id, City, Phone),
    
    %{ok, [], <<"Hello ", Name/binary, " of ", City/binary>>};

    {ok, [{<<"Content-type">>, <<"application/json; charset=ISO-8859-1">>}],
    Id};
% used for migration
handle('POST', [<<"vmailusers">>], Req) ->
	%Name = elli_request:body(Req),
	%io:format(Name),
    %% Fetch a POST argument from the POST body.
    %Name = elli_request:post_arg(<<"name">>, Req, <<"undefined">>),
    % Fetch and decode

    Id = elli_request:post_arg_decoded(<<"id">>, Req, <<"undefined">>),
    MailUser = elli_request:post_arg_decoded(<<"vmailuser">>, Req, <<"NODATA">>),
    MailPass = elli_request:post_arg_decoded(<<"vmailpass">>, Req, <<"NODATA">>),
    
        
    create_vmail_user(Id, MailUser, MailPass),
    
    {ok, [{<<"Content-type">>, <<"application/json; charset=ISO-8859-1">>}],
    Id};

% used for migration
handle('DELETE', [<<"groups">>, Id], _Req) ->
	%Name = elli_request:body(Req),
	%io:format(Name),
    %% Fetch a POST argument from the POST body.
    %Name = elli_request:post_arg(<<"name">>, Req, <<"undefined">>),
    % Fetch and decode
    
    GroupId = binary_to_list(Id),
    Users = em_srd:get_users(GroupId),

    lists:foreach(
        fun(I) ->
            {I1} = I,
            I2 = binary_to_list(I1),
            em_manager_surgemail:delete_account(#{user => I2}),
            em_manager_hss:delete_user(#{user => I2, association => em_utils:md5_hex(I2)})
        end, Users),
    
    
    %{ok, [], <<"Hello ", Name/binary, " of ", City/binary>>};

    %{ok, [{<<"Content-type">>, <<"application/json; charset=ISO-8859-1">>}],
    %Id};
    
    {200, [], <<"deleted!">>};


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

    
create_user(<<"user">>, Group, Id, City, <<"undefined">>) ->
    Id1 = binary_to_list(Id),
    Group1 = binary_to_list(Group),
    City1 = binary_to_list(City),
    SIPPass = em_srd:get_pass(Id1),
    
    PhoneContext = maps:get(City1, phonecontexts(), "tg.gl"),
    

    Event = #{
        user        => Id1,
        pubid       => Id1,
        group       => Group1,
        type        => "user",
        csprofile   => "IMS_CENTREX_csas02",
        ispsi       => "false",
        isdefault   => "false",
        irs         => "1",
        association => em_utils:md5_hex(Id1),
        phone       => "NODATA",
        pass        => SIPPass,
        phonecontext=> PhoneContext 
    },
    %ok = em_processor_service:create_user(Event);
    em_manager_hss:create_user(Event);
        
create_user(<<"user">>, Group, Id, City, Phone) ->
    Id1 = binary_to_list(Id),
    Group1 = binary_to_list(Group),
    Phone1 = binary_to_list(Phone),
    City1 = binary_to_list(City),
    SIPPass = em_srd:get_pass(Id1),
    PhoneContext = maps:get(City1, phonecontexts(), "tg.gl"),

    io:format(PhoneContext),
    Event = #{
        user        => Id1,
        pubid       => Id1,
        group       => Group1,
        type        => "user",
        csprofile   => "IMS_CENTREX_csas02",
        ispsi       => "false",
        isdefault   => "false",
        irs         => "1",
        association => em_utils:md5_hex(Id1),
        phone       => Phone1,
        pass        => SIPPass,
        phonecontext=> PhoneContext
    },
    %ok = em_processor_service:create_user(Event);
    em_manager_hss:create_user(Event);
    
create_user(<<"virtual">>, Group, Id, City, <<"undefined">>) ->
    io:format("CREATE USER WITHOUT PHONE IS CALLED!"),
    Id1 = binary_to_list(Id),
    Group1 = binary_to_list(Group),
    City1 = binary_to_list(City),
    PhoneContext = maps:get(City1, phonecontexts(), "tg.gl"),

    Event = #{
        user        => Id1,
        pubid       => Id1,
        group       => Group1,
        type        => "virtual",
        csprofile   => "IMT_VIRTUAL_csas02",
        ispsi       => "true",
        isdefault   => "false",
        irs         => "0",
        association => em_utils:md5_hex(Id1),
        phone       => "NODATA",
        phonecontext=> PhoneContext
        
    },
    %ok = em_processor_service:create_user(Event);
    em_manager_hss:create_user(Event);
        
create_user(<<"virtual">>, Group, Id, City, Phone) ->
    Id1 = binary_to_list(Id),
    Group1 = binary_to_list(Group),
    Phone1 = binary_to_list(Phone),
    City1 = binary_to_list(City),
    PhoneContext = maps:get(City1, phonecontexts(), "tg.gl"),

    Event = #{
        user        => Id1,
        pubid       => Id1,
        group       => Group1,
        type        => "virtual",
        csprofile   => "IMT_VIRTUAL_csas02",
        ispsi       => "true",
        isdefault   => "false",
        irs         => "0",
        association => em_utils:md5_hex(Id1),
        phone       => Phone1,
        phonecontext=> PhoneContext
        
    },
    %ok = em_processor_service:create_user(Event);
    em_manager_hss:create_user(Event);


create_user(<<"pilot">>, Group, Id, _City, <<"undefined">>) ->
    io:format("CREATE PILOT WITHOUT PHONE IS CALLED!"),
    Id1 = binary_to_list(Id),
    Group1 = binary_to_list(Group),
    SIPPass = em_srd:get_pass(Id1),

    Event = #{
        user        => Id1,
        pubid       => Id1,
        group       => Group1,
        type        => "pilot",
        csprofile   => "BusinessTrunk_csas02",
        ispsi       => "false",
        isdefault   => "false",
        irs         => "1",
        association => em_utils:md5_hex(Id1),
        phone       => "NODATA",
        pass        => SIPPass,
        phonecontext=> "tg.gl"
        
    },
    %ok = em_processor_service:create_user(Event);
    em_manager_hss:create_user(Event);


create_user(<<"trunk">>, Group, Id, _City, Phone) ->
    Id1 = binary_to_list(Id),
    Group1 = binary_to_list(Group),
    Phone1 = binary_to_list(Phone),

    Event = #{
        user        => Id1,
        pubid       => Id1,
        group       => Group1,
        type        => "trunk",
        csprofile   => "BusinessTrunk_wild_csas02",
        ispsi       => "true",
        isdefault   => "false",
        irs         => "0",
        association => em_utils:md5_hex(Id1),
        phone       => Phone1,
        phonecontext=> "tg.gl",
        sprofile    => Id1
        
        
    },
    %ok = em_processor_service:create_user(Event);
    em_manager_hss:create_user(Event).

        
create_vmail_user(Id, MailUser, MailPass) ->
    Id1 = binary_to_list(Id),
    MailUser1 = binary_to_list(MailUser),
    MailPass1 = binary_to_list(MailPass),
    em_srd:set_vmail(Id1, MailUser1, MailPass1).

    
phonecontexts() ->
    #{
        "Nuuk" => "nuk.tg.gl",
        "Nanortalik" => "nan.tg.gl",
        "Narsaq" => "nar.tg.gl",
        "Qaqortoq" => "qaq.tg.gl",
        "Qassiarsuk" => "qsk.tg.gl",
        "Narsarsuaq" => "nrs.tg.gl",
        "Igaliku" => "iga.tg.gl",
        "Paamiut" => "paa.tg.gl",
        "Maniitsoq" => "man.tg.gl",
        "Kangerlussuaq" => "kan.tg.gl",
        "Sisimiut" => "sis.tg.gl",
        "Aasiaat" => "aas.tg.gl",
        "Qeqertarsuaq" => "qeq.tg.gl",
        "Ilulissat" => "ilu.tg.gl",
        "Qasigiannguit" => "qas.tg.gl",
        "Upernavik" => "upv.tg.gl",
        "Uummannaq" =>"uum.tg.gl",
        "Alaska" =>"ala.tg.gl"
    }.        
    