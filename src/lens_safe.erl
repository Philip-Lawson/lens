-module(lens_safe).

-export([get/2,
         getter/1,
         set/3,
         setter/1,
         transform/3,
         accessors/1]).

%% API

get({ok, NestedRecord}, PositionList) ->
    get(NestedRecord, PositionList);
get({error, _} = Error, _) ->
    Error;
get(NestedRecord, PositionList) ->
    F = fun() ->
            lens:get(NestedRecord, PositionList)
    end,
    try_unsafe(F).

getter(PositionList) when is_list(PositionList) ->
    fun(NestedRecord) -> get(NestedRecord, PositionList) end.

set({ok, NestedRecord}, PositionList, Element) ->
    set(NestedRecord, PositionList, Element);
set({error, _} = Error, _, _) ->
    Error;
set(Record, PositionList, Element) ->
    F = fun() -> 
            lens:set(Record, PositionList, Element) 
    end,
    try_unsafe(F).

setter(PositionList) when is_list(PositionList) ->
    fun(NestedRecord, Element) -> 
        set(NestedRecord, PositionList, Element)
    end.

transform(NestedRecord, PositionList, F) ->
    Fun = fun() ->
            lens:transform(NestedRecord, PositionList, F)
    end,
    try_unsafe(Fun).

try_unsafe(F) ->
    try 
        {ok, F()}
    catch
        error:_ ->
            {error, not_found}
    end.

accessors(PositionList) ->
  {getter(PositionList), setter(PositionList)}.
