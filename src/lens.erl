-module(lens).


-export([get/2,
         getter/1,
         set/3,
         setter/1,
         transform/3,
         accessors/1]).

%% API

get(NestedRecord, PositionList) ->
    lists:foldl(fun(X, Acc) -> element(X + 1, Acc) end,
                NestedRecord, PositionList).

getter(PositionList) when is_list(PositionList) ->
    fun(NestedRecord) -> get(NestedRecord, PositionList) end.

set(Record, [N], Element) ->
    setelement(N + 1, Record, Element);
set(NestedRecord, [H|T], Element) -> 
    setelement(H + 1, NestedRecord,
               set(get(NestedRecord, [H]), T, Element)).

setter(PositionList) when is_list(PositionList) ->
    fun(NestedRecord, Element) -> 
        set(NestedRecord, PositionList, Element)
    end.

transform(Record, [N], F) ->
    Value = element(N + 1, Record),
    setelement(N + 1, Record, F(Value));
transform(NestedRecord, [H|T], F) ->
    setelement(H + 1, NestedRecord, 
               transform(get(NestedRecord, [H]), T, F)). 

accessors(PositionList) ->
  {getter(PositionList), setter(PositionList)}.
