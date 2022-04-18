module Data.Article exposing (ArticleMetadata, ArticleTime(..), ArticleWithMetadata, LightArticle(..), codec, list, listWithMetadata, metadataDecoder, slugToFilePath, tags)

import Data.Tag as Tag exposing (Tag)
import DataSource exposing (DataSource)
import DataSource.File
import DataSource.Glob as Glob
import Date
import Iso8601
import List.Extra as List
import OptimizedDecoder as Decode exposing (Decoder)
import Serialize as Codec exposing (Codec)
import Time


type alias ArticleWithMetadata =
    { slug : String
    , metadata : ArticleMetadata
    }


type alias ArticleMetadata =
    { title : String
    , tags : List Tag
    , priority : Int
    , datePublished : Maybe ArticleTime
    , dateUpdated : Maybe ArticleTime
    }


type ArticleTime
    = Iso8601 Time.Posix
    | Date Date.Date


codec : Codec () ArticleWithMetadata
codec =
    Codec.record ArticleWithMetadata
        |> Codec.field .slug Codec.string
        |> Codec.field .metadata metadataCodec
        |> Codec.finishRecord


metadataCodec : Codec () ArticleMetadata
metadataCodec =
    Codec.record ArticleMetadata
        |> Codec.field .title Codec.string
        |> Codec.field .tags tagListCodec
        |> Codec.field .priority Codec.int
        |> Codec.field .datePublished (Codec.maybe articleTimeCodec)
        |> Codec.field .dateUpdated (Codec.maybe articleTimeCodec)
        |> Codec.finishRecord


tagListCodec : Codec () (List Tag)
tagListCodec =
    Codec.map
        (List.filterMap Tag.fromString)
        (List.map Tag.name)
        (Codec.list Codec.string)


articleTimeCodec : Codec () ArticleTime
articleTimeCodec =
    Codec.customType
        (\fiso fdate value ->
            case value of
                Iso8601 i ->
                    fiso i

                Date d ->
                    fdate d
        )
        |> Codec.variant1 Iso8601 isoCodec
        |> Codec.variant1 Date dateCodec
        |> Codec.finishCustomType


isoCodec : Codec () Time.Posix
isoCodec =
    Codec.map Time.millisToPosix Time.posixToMillis Codec.int


dateCodec : Codec () Date.Date
dateCodec =
    Codec.map Date.fromRataDie Date.toRataDie Codec.int


type LightArticle
    = ArticleHtml { slug : String }
    | ArticleMarkdown { slug : String }
    | ArticleLink { slugs : List String }


list : DataSource (List LightArticle)
list =
    let
        markdowns =
            Glob.succeed (\slug -> ArticleMarkdown { slug = slug })
                |> Glob.match (Glob.literal "articles/")
                |> Glob.capture Glob.wildcard
                |> Glob.match (Glob.literal ".md")
                |> Glob.toDataSource

        htmls =
            Glob.succeed (\slug -> ArticleHtml { slug = slug })
                |> Glob.match (Glob.literal "articles/")
                |> Glob.capture Glob.wildcard
                |> Glob.match (Glob.literal ".html")
                |> Glob.toDataSource

        links =
            Glob.succeed
                (\slug ->
                    ArticleLink
                        { slugs =
                            let
                                _ =
                                    Debug.todo
                            in
                            [ slug ]
                        }
                )
                |> Glob.match (Glob.literal "redirects/")
                |> Glob.capture Glob.wildcard
                |> Glob.toDataSource
    in
    DataSource.map3 (\m h l -> m ++ h ++ l)
        markdowns
        htmls
        links


listWithMetadata : DataSource (List ArticleWithMetadata)
listWithMetadata =
    list
        |> DataSource.andThen
            (\articles ->
                articles
                    |> List.map fetchMetadata
                    |> DataSource.combine
                    |> DataSource.map List.concat
            )
        |> (-- FIXME
            if False then
                DataSource.distillSerializeCodec "articles" (Codec.list codec)

            else
                identity
           )


fetchMetadata : LightArticle -> DataSource (List ArticleWithMetadata)
fetchMetadata article =
    case article of
        ArticleHtml { slug } ->
            slugToFilePath slug
                |> DataSource.andThen (.path >> DataSource.File.onlyFrontmatter metadataDecoder)
                |> DataSource.map
                    (\metadata ->
                        [ { slug = slug
                          , metadata = metadata
                          }
                        ]
                    )

        ArticleMarkdown { slug } ->
            slugToFilePath slug
                |> DataSource.andThen (.path >> DataSource.File.onlyFrontmatter metadataDecoder)
                |> DataSource.map
                    (\metadata ->
                        [ { slug = slug
                          , metadata = metadata
                          }
                        ]
                    )

        ArticleLink slugs ->
            let
                _ =
                    Debug.todo
            in
            DataSource.succeed []


tags : DataSource (List ( Tag, Int ))
tags =
    listWithMetadata
        |> DataSource.map
            (\articles ->
                articles
                    |> List.concatMap (.metadata >> .tags)
                    |> List.gatherEqualsBy Tag.toSlug
                    |> List.map (\( tag, copies ) -> ( tag, 1 + List.length copies ))
            )
        |> (-- FIXME
            -- if False then
            --     DataSource.distillSerializeCodec "tags" (Codec.list <| Codec.tuple Codec.string Codec.int)
            -- else
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
        |> Decode.andMap (Decode.maybe <| Decode.field "date-published" timeDecoder)
        |> Decode.andMap (Decode.maybe <| Decode.field "date-updated" timeDecoder)


timeDecoder : Decoder ArticleTime
timeDecoder =
    Decode.string
        |> Decode.andThen
            (\raw ->
                case Iso8601.toTime raw of
                    Ok posix ->
                        Decode.succeed <| Iso8601 posix

                    Err _ ->
                        case List.map String.toInt <| String.split "/" (String.trim raw) of
                            [ Just m, Just d, Just y ] ->
                                Decode.succeed <| Date <| Date.fromCalendarDate y (Date.numberToMonth m) d

                            _ ->
                                Decode.fail "Invalid time"
            )


tagsDecoder : Decoder (List Tag)
tagsDecoder =
    Decode.map (String.split " " >> List.filterMap Tag.fromString)
        Decode.string
