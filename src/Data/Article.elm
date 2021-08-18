module Data.Article exposing (Article, ArticleMetadata, codec, list, listWithMetadata, metadataDecoder, slugToFilePath, tags)

import DataSource exposing (DataSource)
import DataSource.File
import DataSource.Glob as Glob
import Iso8601
import List.Extra as List
import OptimizedDecoder as Decode exposing (Decoder)
import Serialize as Codec exposing (Codec)
import Time exposing (Posix)


type alias Article =
    { slug : String
    , metadata : ArticleMetadata
    }


type alias ArticleMetadata =
    { title : String
    , tags : List String
    , priority : Int
    , datePublished : Maybe Posix
    , dateUpdated : Maybe Posix
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
        |> Codec.field .datePublished dateCodec
        |> Codec.field .dateUpdated dateCodec
        |> Codec.finishRecord


dateCodec : Codec () (Maybe Posix)
dateCodec =
    Codec.maybe (Codec.map Time.millisToPosix Time.posixToMillis Codec.int)


list : DataSource (List { slug : String, isMarkdown : Bool })
list =
    let
        markdowns =
            Glob.succeed (\slug -> { slug = slug, isMarkdown = True })
                |> Glob.match (Glob.literal "articles/")
                |> Glob.capture Glob.wildcard
                |> Glob.match (Glob.literal ".md")
                |> Glob.toDataSource

        htmls =
            Glob.succeed (\slug -> { slug = slug, isMarkdown = False })
                |> Glob.match (Glob.literal "articles/")
                |> Glob.capture Glob.wildcard
                |> Glob.match (Glob.literal ".html")
                |> Glob.toDataSource
    in
    DataSource.map2 (++)
        markdowns
        htmls


listWithMetadata : DataSource (List Article)
listWithMetadata =
    list
        |> DataSource.andThen
            (\slugs ->
                slugs
                    |> List.map
                        (\{ slug } ->
                            slugToFilePath slug
                                |> DataSource.andThen (.path >> DataSource.File.onlyFrontmatter metadataDecoder)
                                |> DataSource.map
                                    (\metadata ->
                                        { slug = slug
                                        , metadata = metadata
                                        }
                                    )
                        )
                    |> DataSource.combine
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


slugToFilePath : String -> DataSource { path : String, isMarkdown : Bool }
slugToFilePath slug =
    let
        mdPath =
            "articles/" ++ slug ++ ".md"

        markdowns =
            Glob.succeed { path = mdPath, isMarkdown = True }
                |> Glob.match (Glob.literal mdPath)
                |> Glob.toDataSource

        htmlPath =
            "articles/" ++ slug ++ ".html"

        htmls =
            Glob.succeed { path = htmlPath, isMarkdown = False }
                |> Glob.match (Glob.literal htmlPath)
                |> Glob.toDataSource
    in
    DataSource.map2
        (\m h -> List.head <| m ++ h)
        markdowns
        htmls
        |> DataSource.andThen
            (\h ->
                case h of
                    Nothing ->
                        DataSource.fail "Article not found"

                    Just c ->
                        DataSource.succeed c
            )


metadataDecoder : Decoder ArticleMetadata
metadataDecoder =
    Decode.succeed ArticleMetadata
        |> Decode.andMap (Decode.field "title" Decode.string)
        |> Decode.andMap (Decode.field "tags" tagsDecoder)
        |> Decode.andMap (Decode.field "priority" Decode.int)
        |> Decode.andMap (Decode.maybe <| Decode.field "date-published" dateDecoder)
        |> Decode.andMap (Decode.maybe <| Decode.field "date-updated" dateDecoder)


dateDecoder : Decoder Posix
dateDecoder =
    Decode.string
        |> Decode.andThen
            (\raw ->
                case Iso8601.toTime raw of
                    Ok posix ->
                        Decode.succeed posix

                    Err _ ->
                        Decode.fail "Invalid time"
            )


tagsDecoder : Decoder (List String)
tagsDecoder =
    Decode.map (String.split " ")
        Decode.string
