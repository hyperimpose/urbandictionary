-module(urbandictionary).

-include_lib("kernel/include/logger.hrl").


-export([autocomplete/1, autocomplete/2,
         define/1, define/2,
         random/0,
         words_of_the_day/0, words_of_the_day/1]).


%%% Macros
-define(JSON(Req), begin
                       {ok, {{_, 200, _}, _, Body}} = Req,
                       djson:decode(unicode:characters_to_binary(Body))
                   end).


%%%===================================================================
%%% autocomplete
%%%===================================================================

-spec autocomplete(Term :: iodata()) -> [binary()].

autocomplete(Term) -> autocomplete(Term, #{extra => false}).


-spec autocomplete(Term :: iodata(), Opts :: #{extra := boolean()})
                  -> [binary()] | [#{binary() => binary()}].

autocomplete(Term, #{extra := false}) ->
    U = "https://api.urbandictionary.com/v0/autocomplete",
    Q = uri_string:compose_query([{"term", Term}]),
    ?JSON(httpc:request([U, "?", Q]));
autocomplete(Term, #{extra := true}) ->
    U = "https://api.urbandictionary.com/v0/autocomplete-extra",
    Q = uri_string:compose_query([{"term", Term}]),
    J = ?JSON(httpc:request([U, "?", Q])),
    map_get(<<"results">>, J).


%%%===================================================================
%%% define
%%%===================================================================

-spec define(Query :: iodata()) -> [#{binary() => term()}].

define(Query) -> define(Query, #{query => term}).


-spec define(Query :: iodata(), Opts :: #{atom() => term()})
            -> [#{binary() => term()}].

define(Query, Opts) ->
    Page = case maps:get(page, Opts, "1") of
               P when is_integer(P) -> integer_to_list(P);
               P                    -> P
           end,
    case Opts of
        #{query := defid} -> define_defid(Query, Page);
        _Else             -> define_term(Query, Page)
    end.

define_term(Term, Page) ->
    Q = uri_string:compose_query([{"term", Term}, {"page", Page}]),
    J = ?JSON(httpc:request(["https://api.urbandictionary.com/v0/define?", Q])),
    map_get(<<"list">>, J).

define_defid(Query, Page) ->
    Defid = if
                is_integer(Query) -> integer_to_list(Query);
                true              -> Query
            end,
    Q = uri_string:compose_query([{"defid", Defid}, {"page", Page}]),
    J = ?JSON(httpc:request(["https://api.urbandictionary.com/v0/define?", Q])),
    map_get(<<"list">>, J).


%%%===================================================================
%%% random
%%%===================================================================

-spec random() -> [#{binary() => term()}].

random() ->
    J = ?JSON(httpc:request("https://api.urbandictionary.com/v0/random")),
    map_get(<<"list">>, J).


%%%===================================================================
%%% words of the day
%%%===================================================================

-spec words_of_the_day() -> [#{binary() => term()}].

words_of_the_day() -> words_of_the_day(#{}).


-spec words_of_the_day(Opts :: map()) -> [#{binary() => term()}].

words_of_the_day(Opts) ->
    Page = case maps:get(page, Opts, "1") of
               P when is_integer(P) -> integer_to_list(P);
               P                    -> P
           end,
    U = ["https://api.urbandictionary.com/v0/words_of_the_day?",
         uri_string:compose_query([{"page", Page}])],
    try
        J = ?JSON(httpc:request(U)),
        map_get(<<"list">>, J)
    catch
        error:{badmatch, {ok, {{"HTTP/1.1", 400, _}, _, _}} = E} ->
            %% This usually happens when a page that does not exist is given.
            %% For better API we just return an empty list.
            ?LOG_DEBUG("[urbandictionary] 400 Bad Request",
                       #{response => E, url => U}),
            [];
        error:{badmatch, {ok, {{"HTTP/1.1", 500, _}, _, _}} = E} ->
            %% This usually happens when a page that does not exist is given.
            %% For better API we just return an empty list.
            ?LOG_DEBUG("[urbandictionary] 500 Internal Server Error",
                       #{response => E, url => U}),
            []
    end.
