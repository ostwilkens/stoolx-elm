module Socket exposing (Socket(..), getId, getIndex)


type Socket
    = Input Int Int
    | Output Int Int


getId : Socket -> Int
getId socket =
    case socket of
        Input id _ ->
            id

        Output id _ ->
            id


getIndex : Socket -> Int
getIndex socket =
    case socket of
        Input _ index ->
            index

        Output _ index ->
            index
