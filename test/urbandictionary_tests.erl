-module(urbandictionary_tests).

-include_lib("eunit/include/eunit.hrl").



%%%===================================================================
%%% autocomplete
%%%===================================================================

autocomplete_test() ->
    L = urbandictionary:autocomplete("irc"),
    ?assert(is_list(L)),
    ?assert(length(L) > 1),
    ?assert(lists:member(<<"IRC">>, L)).


autocomplete_extra_test() ->
    L = urbandictionary:autocomplete("irc", #{extra => true}),
    ?assert(is_list(L)),
    ?assert(length(L) > 1),
    FormatChk = fun (#{<<"term">> := T, <<"preview">> := P}) ->
                        is_binary(T) andalso is_binary(P);
                    (_Else)                                  ->
                        false
                end,
    ?assert(lists:all(FormatChk, L)).


%%%===================================================================
%%% define
%%%===================================================================

-define(IS_VALID_DEFINE, fun (#{<<"author">>       := Author,
                                <<"current_vote">> := CurrentVote,
                                <<"defid">>        := DefId,
                                <<"definition">>   := Definition,
                                <<"example">>      := Example,
                                <<"permalink">>    := Permalink,
                                <<"thumbs_down">>  := ThumbsDown,
                                <<"thumbs_up">>    := ThumbsUp,
                                <<"word">>         := Word,
                                <<"written_on">>   := WrittenOn}) ->
                                 is_binary(Author)
                                     andalso is_binary(CurrentVote)
                                     andalso is_integer(DefId)
                                     andalso is_binary(Definition)
                                     andalso is_binary(Example)
                                     andalso is_binary(Permalink)
                                     andalso is_integer(ThumbsDown)
                                     andalso is_integer(ThumbsUp)
                                     andalso is_binary(Word)
                                     andalso is_binary(WrittenOn);
                             (_Else) ->
                                 false
                         end).


define_test() ->
    L1 = urbandictionary:define("irc"),
    ?assert(is_list(L1)),
    ?assert(length(L1) > 1),
    ?assert(lists:all(?IS_VALID_DEFINE, L1)),

    L2 = urbandictionary:define("irc", #{page => 2}),
    ?assert(is_list(L2)),
    ?assert(length(L2) > 1),
    ?assert(lists:all(?IS_VALID_DEFINE, L2)).


define_not_found_test() ->
    L1 = urbandictionary:define("asdlkfjsdlfdslfsdalfj"),
    ?assertEqual(L1, []),

    L2 = urbandictionary:define("irc", #{page => 200}),
    ?assertEqual(L2, []).


define_defid_test() ->
    L = urbandictionary:define(718912, #{query => defid}),
    ?assert(is_list(L)),
    ?assert(length(L) == 1),
    ?assert(lists:all(?IS_VALID_DEFINE, L)).


define_defid_not_found_test() ->
    L1 = urbandictionary:define("asdlkfjsdlfdslfsdalfj", #{query => defid}),
    ?assertEqual(L1, []),
    L2 = urbandictionary:define("0", #{query => defid}),
    ?assertEqual(L2, []).


%%%===================================================================
%%% random
%%%===================================================================

random_test() ->
    L = urbandictionary:random(),
    ?assert(is_list(L)),
    ?assert(length(L) > 1),
    ?assert(lists:all(?IS_VALID_DEFINE, L)).


%%%===================================================================
%%% words of the day
%%%===================================================================

-define(IS_VALID_WOTD, fun (#{<<"author">>       := Author,
                              <<"current_vote">> := CurrentVote,
                              <<"date">>         := Date,  % only in wotd
                              <<"defid">>        := DefId,
                              <<"definition">>   := Definition,
                              <<"example">>      := Example,
                              <<"permalink">>    := Permalink,
                              <<"thumbs_down">>  := ThumbsDown,
                              <<"thumbs_up">>    := ThumbsUp,
                              <<"word">>         := Word,
                              <<"written_on">>   := WrittenOn}) ->
                               is_binary(Author)
                                   andalso is_binary(CurrentVote)
                                   andalso is_binary(Date)
                                   andalso is_integer(DefId)
                                   andalso is_binary(Definition)
                                   andalso is_binary(Example)
                                   andalso is_binary(Permalink)
                                   andalso is_integer(ThumbsDown)
                                   andalso is_integer(ThumbsUp)
                                   andalso is_binary(Word)
                                   andalso is_binary(WrittenOn);
                           (_Else) ->
                               false
                       end).


words_of_the_day_test() ->
    L1 = urbandictionary:words_of_the_day(),
    ?assert(is_list(L1)),
    ?assert(length(L1) > 1),
    ?assert(lists:all(?IS_VALID_WOTD, L1)),

    L2 = urbandictionary:words_of_the_day(#{page => 2}),
    ?assert(is_list(L2)),
    ?assert(length(L2) > 1),
    ?assert(lists:all(?IS_VALID_WOTD, L2)),

    L3 = urbandictionary:words_of_the_day(#{page => 9999999}),
    ?assertEqual(L3, []).
