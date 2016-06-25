# lens
``` erlang
7> NestedRecord.
\#person{name = undefined,age = undefined,
    address = #address{abode = #place{building = house,
        rented = undefined},
            number = undefined,street = undefined}}
8> lens:get(NestedRecord, [3,1,1]).
            house
9> lens:set(NestedRecord, [3,1,1], apartment).
\#person{name = undefined,age = undefined,
    address = #address{abode = #place{building = apartment,
        rented = undefined},
            number = undefined,street = undefined}}
10> {Getter, Setter} = lens:accessors([3,1,1]).
{#Fun<lens.1.76412078>,#Fun<lens.2.76412078>}
11> Getter(NestedRecord).
house
12> NewRecord = Setter(NestedRecord, apartment).
\#person{name = undefined,age = undefined,
    address = #address{abode = #place{building = apartment,
        rented = undefined},
            number = undefined,street = undefined}}
13> Stringy = lens:transform(NewRecord, [3,1,1], fun(X) -> atom_to_list(X) end).
\#person{name = undefined,age = undefined,
    address = #address{abode = #place{building = "apartment",
        rented = undefined},
            number = undefined,street = undefined}}
```
