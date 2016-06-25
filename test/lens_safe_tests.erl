-module(lens_safe_tests).

%% Test cases

-record(place, {building, rented}).
-record(address, {abode = #place{}, number, street}). 
-record(person, {name, age, address = #address{}}).

-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Test cases
%%%===================================================================

get_test() ->
    Value = apartment,
    NestedRecord = make_record(Value),
    ?assertEqual(lens_safe:get(NestedRecord, [3,1,1]), {ok, Value}).

get_error_test() ->
    NestedRecord = make_record(1),
    Expected = expected_error(),
    Actual = lens_safe:get(NestedRecord, [3,1,1,1]),
    ?assertEqual(Expected, Actual).

set_test() ->
    Value = house,
    NestedRecord = make_record(apartment),
    Expected = {ok, Value},
    {ok, NewRecord} = lens_safe:set(NestedRecord, [3,1,1], Value), 
    Actual = lens_safe:get(NewRecord, [3,1,1]),
    ?assertEqual(Expected, Actual).

set_error_test() ->
    NestedRecord = make_record(1),
    Expected = expected_error(),
    Actual = lens_safe:set(NestedRecord, [20], 1),
    ?assertEqual(Expected, Actual).

get_set_short_circuit_test() ->
    Error = expected_error(),
    ?assertEqual(Error, lens_safe:get(Error, [1])),
    ?assertEqual(Error, lens_safe:set(Error, [1], 1)).

get_set_ok_test() ->
    Value = house,
    NewValue = apartment,
    NestedRecord = {ok, make_record(Value)},
    NewRecord = {ok, make_record(NewValue)},
    ?assertEqual({ok, Value}, lens_safe:get(NestedRecord, [3,1,1])),
    ?assertEqual(NewRecord, lens_safe:set(NestedRecord, [3,1,1], NewValue)).

getter_test() ->
    Value = apartment,
    NestedRecord = make_record(Value),
    Getter = lens_safe:getter([3,1,1]),
    ?assertEqual(Getter(NestedRecord), {ok, Value}).
    
setter_test() ->
    Value = house,
    NestedRecord = make_record(apartment),
    Expected = {ok, Value},
    Setter = lens_safe:setter([3,1,1]),
    {ok, NewRecord} = Setter(NestedRecord, Value),
    Actual = lens_safe:get(NewRecord, [3,1,1]),
    ?assertEqual(Expected, Actual).

accessor_test() ->
    Value = hoos,
    {Getter, Setter} = lens_safe:accessors([3,1,1]),
    NestedRecord = make_record(not_hoos),
    ?assertEqual(Getter(Setter(NestedRecord, Value)), {ok, Value}).

bad_accessor_test() ->
    Value = hoos,
    {Getter, Setter} = lens_safe:accessors([3,1,100]),
    NestedRecord = make_record(not_hoos),
    ?assertEqual(Getter(Setter(NestedRecord, Value)), expected_error()).

transform_test() ->
    Value = 1,
    F = fun(X) -> X + 1 end,
    NestedRecord = make_record(Value),
    TransformedRecord = lens_safe:transform(NestedRecord, [3,1,1], F),
    ?assertEqual(lens_safe:get(TransformedRecord, [3,1,1]), {ok, F(Value)}).

make_record(Value) ->
    #person{address =
            #address{abode = 
                     #place{building = Value}}}.

expected_error() -> {error, not_found}.
