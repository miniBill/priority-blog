module Data.Tag exposing (Tag, fromString, name, toSlug)

import Slug exposing (Slug)


type Tag
    = Tag String Slug


fromString : String -> Maybe Tag
fromString rawName =
    Slug.generate rawName
        |> Maybe.map (Tag rawName)


toSlug : Tag -> String
toSlug (Tag _ slug) =
    Slug.toString slug


name : Tag -> String
name (Tag n _) =
    n
