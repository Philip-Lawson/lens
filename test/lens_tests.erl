-module(lens_tests).

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
    ?assertEqual(lens:get(NestedRecord, [3,1,1]), Value).

get_error_test() ->
    NestedRecord = make_record(1),
    ?assertError(badarg, lens:get(NestedRecord, [3,1,1,1])).

set_test() ->
    Value = house,
    NestedRecord = make_record(apartment),
    NewRecord = lens:set(NestedRecord, [3,1,1], Value), 
    ?assertEqual(Value, lens:get(NewRecord, [3,1,1])).

set_error_test() ->
    NestedRecord = make_record(1),
    ?assertError(badarg, lens:set(NestedRecord, [20], 1)).

getter_test() ->
    Value = apartment,
    NestedRecord = make_record(Value),
    Getter = lens:getter([3,1,1]),
    ?assertEqual(Getter(NestedRecord), Value).
    
setter_test() ->
    Value = house,
    NestedRecord = make_record(apartment),
    Setter = lens:setter([3,1,1]),
    NewRecord = Setter(NestedRecord, Value),
    ?assertEqual(Value, lens:get(NewRecord, [3,1,1])).

accessor_test() ->
    Value = hoos,
    {Getter, Setter} = lens:accessors([3,1,1]),
    NestedRecord = make_record(not_hoos),
    ?assertEqual(Getter(Setter(NestedRecord, Value)), Value).

bad_accessor_test() ->
    Value = hoos,
    {Getter, Setter} = lens:accessors([3,1,100]),
    NestedRecord = make_record(not_hoos),
    ?assertError(badarg, Getter(Setter(NestedRecord, Value))).

transform_test() ->
    Value = 1,
    F = fun(X) -> X + 1 end,
    NestedRecord = make_record(Value),
    TransformedRecord = lens:transform(NestedRecord, [3,1,1], F),
    ?assertEqual(lens:get(TransformedRecord, [3,1,1]), F(Value)).

make_record(Value) ->
    #person{address =
            #address{abode = 
                     #place{building = Value}}}.
