module SetExtras exposing (..)

import Set exposing (..)


setFilterMap : (comparable -> Maybe comparable) -> Set comparable -> Set comparable
setFilterMap filter set =
    let
        helper value set =
            case filter value of
                Nothing ->
                    set

                Just newValue ->
                    insert newValue set
    in
        foldl helper Set.empty set


setConcatMap : (comparable -> Set comparable) -> Set comparable -> Set comparable
setConcatMap mapfunc set =
    let
        helper value set =
            Set.union (mapfunc value) set
    in
        foldl helper Set.empty set
