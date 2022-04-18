module Data.Tag exposing (Tag, fromString, listCodec, name, toSlug)

import Serialize as Codec exposing (Codec)
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


listCodec : Codec () (List Tag)
listCodec =
    Codec.map
        (List.filterMap fromString)
        (List.map name)
        (Codec.list Codec.string)
