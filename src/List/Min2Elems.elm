module List.Min2Elems exposing (..)

import List.Extra


type alias ListMin2Elems a =
    { head : a
    , neck : a
    , rest : List a
    }


fromList : List a -> Maybe (ListMin2Elems a)
fromList list =
    case list of
        firstElement :: secondElement :: rest ->
            Just <| withAtLeastTwoElements firstElement secondElement rest

        _ ->
            Nothing


toList : ListMin2Elems a -> List a
toList nonEmptyList =
    nonEmptyList.head :: nonEmptyList.neck :: nonEmptyList.rest


withAtLeastTwoElements : a -> a -> List a -> ListMin2Elems a
withAtLeastTwoElements firstElement secondElement rest =
    { head = firstElement
    , neck = secondElement
    , rest = rest
    }


head : ListMin2Elems a -> a
head =
    .head


neck : ListMin2Elems a -> a
neck =
    .neck


last : ListMin2Elems a -> a
last nonEmptyList =
    List.Extra.last nonEmptyList.rest |> Maybe.withDefault nonEmptyList.neck


member : a -> ListMin2Elems a -> Bool
member a nonEmptyList =
    a == nonEmptyList.head || a == nonEmptyList.neck || List.member a nonEmptyList.rest


length : ListMin2Elems a -> Int
length nonEmptyList =
    List.length nonEmptyList.rest + 2


elemIndex : a -> ListMin2Elems a -> Maybe Int
elemIndex elem nonEmptyList =
    if elem == nonEmptyList.head then
        Just 0
    else if elem == nonEmptyList.neck then
        Just 1
    else
        Maybe.map ((+) 2) <| List.Extra.elemIndex elem nonEmptyList.rest


cons : a -> ListMin2Elems a -> ListMin2Elems a
cons newHead nonEmptyList =
    { nonEmptyList
        | head = newHead
        , neck = nonEmptyList.head
        , rest = nonEmptyList.neck :: nonEmptyList.rest
    }


drop : Int -> ListMin2Elems a -> ListMin2Elems a
drop numElems nonEmptyList =
    -- numElems clamped to: 0 <= numElems <= length - 2
    if numElems <= 0 then
        nonEmptyList
    else
        case nonEmptyList.rest of
            [] ->
                nonEmptyList

            x :: xs ->
                drop (numElems - 1)
                    { nonEmptyList
                        | head = nonEmptyList.neck
                        , neck = x
                        , rest = xs
                    }


take : Int -> ListMin2Elems a -> ListMin2Elems a
take numElems nonEmptyList =
    let
        numToDrop =
            (length nonEmptyList) - numElems
    in
        dropLastN numToDrop nonEmptyList


dropLast : ListMin2Elems a -> ListMin2Elems a
dropLast nonEmptyList =
    dropLastN 1 nonEmptyList


dropLastN : Int -> ListMin2Elems a -> ListMin2Elems a
dropLastN numElems nonEmptyList =
    { nonEmptyList | rest = dropLastNOfList numElems nonEmptyList.rest }


dropLastNOfList : Int -> List a -> List a
dropLastNOfList n l =
    let
        numToKeep =
            (List.length l) - n |> max 0
    in
        List.take numToKeep l
