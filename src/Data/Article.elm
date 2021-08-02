module Data.Article exposing (Article, ArticleMetadata, codec, list, listWithMetadata, metadataDecoder, slugToFilePath, tags)

import DataSource exposing (DataSource)
import DataSource.File
import DataSource.Glob as Glob
import List.Extra as List
import OptimizedDecoder as Decode exposing (Decoder)
import Serialize as Codec exposing (Codec)


type alias Article =
    { slug : String
    , metadata : ArticleMetadata
    }


type alias ArticleMetadata =
    { title : String
    , tags : List String
    , priority : Int
    }


codec : Codec () Article
codec =
    Codec.record Article
        |> Codec.field .slug Codec.string
        |> Codec.field .metadata metadataCodec
        |> Codec.finishRecord


metadataCodec : Codec () ArticleMetadata
metadataCodec =
    Codec.record ArticleMetadata
        |> Codec.field .title Codec.string
        |> Codec.field .tags (Codec.list Codec.string)
        |> Codec.field .priority Codec.int
        |> Codec.finishRecord


list : DataSource (List { slug : String })
list =
    Glob.succeed (\slug -> { slug = slug })
        |> Glob.match (Glob.literal "markdown/articles/")
        |> Glob.capture Glob.wildcard
        |> Glob.match (Glob.literal ".md")
        |> Glob.toDataSource


listWithMetadata : DataSource (List Article)
listWithMetadata =
    list
        |> DataSource.andThen
            (\slugs ->
                slugs
                    |> List.map
                        (\{ slug } ->
                            slugToFilePath slug
                                |> DataSource.File.onlyFrontmatter metadataDecoder
                                |> DataSource.map
                                    (\metadata ->
                                        { slug = slug
                                        , metadata = metadata
                                        }
                                    )
                        )
                    |> DataSource.combine
                    |> DataSource.map (List.sortBy (.metadata >> .priority))
            )
        |> (-- FIXME
            if False then
                DataSource.distillSerializeCodec "articles" (Codec.list codec)

            else
                identity
           )


tags : DataSource (List ( String, Int ))
tags =
    listWithMetadata
        |> DataSource.map
            (\articles ->
                articles
                    |> List.concatMap (.metadata >> .tags)
                    |> List.gatherEquals
                    |> List.map (\( tag, copies ) -> ( tag, 1 + List.length copies ))
            )
        |> (-- FIXME
            if False then
                DataSource.distillSerializeCodec "tags" (Codec.list <| Codec.tuple Codec.string Codec.int)

            else
                identity
           )


slugToFilePath : String -> String
slugToFilePath slug =
    "markdown/articles/" ++ slug ++ ".md"


metadataDecoder : Decoder ArticleMetadata
metadataDecoder =
    Decode.succeed ArticleMetadata
        |> Decode.andMap (Decode.field "title" Decode.string)
        |> Decode.andMap (Decode.field "tags" tagsDecoder)
        |> Decode.andMap (Decode.field "priority" Decode.int)


tagsDecoder : Decoder (List String)
tagsDecoder =
    Decode.map (String.split " ")
        Decode.string
