module ListHelper exposing (..)

import Dict exposing (Dict)


createUniqueList : List String -> List String
createUniqueList list =
    case list of
        [] ->
            []

        x :: [] ->
            x :: []

        x :: xs ->
            if List.member x xs then
                createUniqueList xs

            else
                x :: createUniqueList xs


createTupelList : List a -> List ( a, Int )
createTupelList items =
    List.indexedMap (\i x -> ( x, i )) items


createDict : List String -> Dict String Int
createDict list =
    Dict.fromList (createTupelList list)


extractMaybebVal : List ( Maybe Int, Maybe Int ) -> List ( Int, Int )
extractMaybebVal list =
    List.foldr
        (\mbVal acc ->
            case mbVal of
                ( first, second ) ->
                    case ( first, second ) of
                        ( Just f, Just s ) ->
                            if f == s then
                                acc

                            else
                                ( f, s ) :: acc

                        ( _, Nothing ) ->
                            acc

                        ( Nothing, _ ) ->
                            acc
        )
        []
        list


removeFromList : Int -> List a -> List a
removeFromList i xs =
    List.take i xs ++ List.drop (i + 1) xs
