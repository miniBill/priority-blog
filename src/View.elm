module View exposing (View, map, placeholder)

import Html exposing (Html)


type alias View msg =
    { title : String
    , body : Result String (List (Html msg))
    }


map : (msg1 -> msg2) -> View msg1 -> View msg2
map fn doc =
    { title = doc.title
    , body = Result.map (List.map (Html.map fn)) doc.body
    }


placeholder : String -> View msg
placeholder moduleName =
    { title = "Placeholder - " ++ moduleName
    , body = Err moduleName
    }
