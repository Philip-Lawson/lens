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

set_test() ->
    Value = house,
    NestedRecord = make_record(apartment),
    Expected = Value,
    Actual = lens:get(lens:set(NestedRecord, [3,1,1], Value), [3,1,1]),
    ?assertEqual(Expected, Actual).

getter_test() ->
    Value = apartment,
    NestedRecord = make_record(Value),
    Getter = lens:getter([3,1,1]),
    ?assertEqual(Getter(NestedRecord), Value).
    
setter_test() ->
    Value = house,
    NestedRecord = make_record(apartment),
    Expected = Value,
    Setter = lens:setter([3,1,1]),
    Actual = lens:get(Setter(NestedRecord, Value), [3,1,1]),
    ?assertEqual(Expected, Actual).

accessor_test() ->
    Value = hoos,
    {Getter, Setter} = lens:accessors([3,1,1]),
    NestedRecord = make_record(not_hoos),
    ?assertEqual(Getter(Setter(NestedRecord, Value)), Value).

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
