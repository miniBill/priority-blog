module Data.Article exposing (Article(..), ArticleMetadata, ArticleTime(..), ArticleWithMetadata(..), Redirect, articleFileToFilePath, codec, list, listWithMetadata, metadataDecoder, tags)

import Data.Tag as Tag exposing (Tag)
import DataSource exposing (DataSource)
import DataSource.File
import DataSource.Glob as Glob
import Date
import Iso8601
import List.Extra
import OptimizedDecoder as Decode exposing (Decoder)
import Parser exposing ((|.), (|=), Parser)
import Route exposing (Route(..))
import Serialize as Codec exposing (Codec)
import Time


type Article
    = ArticleFile { slug : String, isMarkdown : Bool }
    | ArticleLink { slug : String, file : String }


type ArticleWithMetadata
    = ArticleFileWithMetadata
        { slug : String
        , metadata : ArticleMetadata
        }
    | ArticleLinkWithMetadata Redirect


type alias Redirect =
    { slug : String
    , url : String
    , metadata : LinkMetadata
    }


type alias ArticleMetadata =
    { title : String
    , tags : List Tag
    , priority : Int
    , datePublished : Maybe ArticleTime
    , dateUpdated : Maybe ArticleTime
    }


type alias LinkMetadata =
    { title : Maybe String
    , tags : List Tag
    , priority : Maybe Int
    , datePublished : Maybe ArticleTime
    , dateUpdated : Maybe ArticleTime
    }


type ArticleTime
    = Iso8601 Time.Posix
    | Date Date.Date


codec : Codec () ArticleWithMetadata
codec =
    Codec.customType
        (\farticle flink value ->
            case value of
                ArticleFileWithMetadata m ->
                    farticle m

                ArticleLinkWithMetadata m ->
                    flink m
        )
        |> Codec.variant1 ArticleFileWithMetadata
            (Codec.record
                (\slug metadata -> { slug = slug, metadata = metadata })
                |> Codec.field .slug Codec.string
                |> Codec.field .metadata articleMetadataCodec
                |> Codec.finishRecord
            )
        |> Codec.variant1 ArticleLinkWithMetadata
            (Codec.record
                (\slug url metadata ->
                    { slug = slug
                    , url = url
                    , metadata = metadata
                    }
                )
                |> Codec.field .slug Codec.string
                |> Codec.field .url Codec.string
                |> Codec.field .metadata linkMetadataCodec
                |> Codec.finishRecord
            )
        |> Codec.finishCustomType


articleMetadataCodec : Codec () ArticleMetadata
articleMetadataCodec =
    Codec.record ArticleMetadata
        |> Codec.field .title Codec.string
        |> Codec.field .tags Tag.listCodec
        |> Codec.field .priority Codec.int
        |> Codec.field .datePublished (Codec.maybe articleTimeCodec)
        |> Codec.field .dateUpdated (Codec.maybe articleTimeCodec)
        |> Codec.finishRecord


linkMetadataCodec : Codec () LinkMetadata
linkMetadataCodec =
    Codec.record LinkMetadata
        |> Codec.field .title (Codec.maybe Codec.string)
        |> Codec.field .tags Tag.listCodec
        |> Codec.field .priority (Codec.maybe Codec.int)
        |> Codec.field .datePublished (Codec.maybe articleTimeCodec)
        |> Codec.field .dateUpdated (Codec.maybe articleTimeCodec)
        |> Codec.finishRecord


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


list : DataSource (List Article)
list =
    let
        markdowns =
            Glob.succeed (\slug -> ArticleFile { slug = slug, isMarkdown = True })
                |> Glob.match (Glob.literal "articles/")
                |> Glob.capture Glob.wildcard
                |> Glob.match (Glob.literal ".md")
                |> Glob.toDataSource

        htmls =
            Glob.succeed (\slug -> ArticleFile { slug = slug, isMarkdown = False })
                |> Glob.match (Glob.literal "articles/")
                |> Glob.capture Glob.wildcard
                |> Glob.match (Glob.literal ".html")
                |> Glob.toDataSource

        links =
            Glob.succeed (\filename -> filename)
                |> Glob.match (Glob.literal "redirects/")
                |> Glob.capture Glob.wildcard
                |> Glob.toDataSource
                |> DataSource.andThen
                    (\filenames ->
                        filenames
                            |> List.map
                                (\filename ->
                                    filename
                                        |> parseRedirectsFile
                                        |> DataSource.map
                                            (List.map
                                                (\{ slug } ->
                                                    ArticleLink
                                                        { slug = slug
                                                        , file = filename
                                                        }
                                                )
                                            )
                                )
                            |> DataSource.combine
                            |> DataSource.map List.concat
                    )
    in
    DataSource.map3 (\m h l -> m ++ h ++ l)
        markdowns
        htmls
        links


parseRedirectsFile : String -> DataSource (List Redirect)
parseRedirectsFile filename =
    DataSource.File.rawFile ("redirects/" ++ filename)
        |> DataSource.map
            (\raw ->
                raw
                    |> String.split "#"
                    |> List.filterMap parseRedirectLine
            )


parseRedirectLine : String -> Maybe Redirect
parseRedirectLine =
    Parser.run redirectLineParser >> Result.toMaybe


redirectLineParser : Parser Redirect
redirectLineParser =
    Parser.succeed
        (\slug url metadata ->
            { slug = String.trim slug
            , url = String.trim url
            , metadata = metadata
            }
        )
        |= Parser.getChompedString (Parser.chompUntil ":")
        |. Parser.symbol ":"
        |. Parser.spaces
        |= Parser.getChompedString (Parser.chompUntilEndOr " ")
        |. Parser.spaces
        |= metadataParser


metadataParser : Parser LinkMetadata
metadataParser =
    let
        empty =
            { title = Nothing
            , tags = []
            , datePublished = Nothing
            , dateUpdated = Nothing
            , priority = Nothing
            }

        attributeParser =
            Parser.succeed (\k v -> ( String.trim k, String.trim v ))
                |= Parser.getChompedString
                    (Parser.chompUntil ":")
                |. Parser.symbol ":"
                |= Parser.getChompedString
                    (Parser.chompWhile (\c -> c /= ';' && c /= ']'))
    in
    Parser.oneOf
        [ Parser.sequence
            { start = "["
            , end = "]"
            , separator = ";"
            , spaces = Parser.spaces
            , trailing = Parser.Optional
            , item = attributeParser
            }
            |> Parser.map
                (\attrs ->
                    attrs
                        |> List.foldl
                            (\( k, v ) acc ->
                                case k of
                                    "priority" ->
                                        case String.toInt v of
                                            Just p ->
                                                { acc | priority = Just p }

                                            Nothing ->
                                                acc

                                    "title" ->
                                        { acc | title = Just v }

                                    "tags" ->
                                        { acc
                                            | tags =
                                                v
                                                    |> String.split " "
                                                    |> List.filterMap Tag.fromString
                                        }

                                    _ ->
                                        -- TODO
                                        acc
                            )
                            empty
                )
        , Parser.succeed empty
        ]


listWithMetadata : DataSource (List ArticleWithMetadata)
listWithMetadata =
    list
        |> DataSource.andThen
            (\articles ->
                articles
                    |> List.map fetchMetadata
                    |> DataSource.combine
            )
        |> (-- FIXME
            if False then
                DataSource.distillSerializeCodec "articles" (Codec.list codec)

            else
                identity
           )


fetchMetadata : Article -> DataSource ArticleWithMetadata
fetchMetadata article =
    case article of
        ArticleLink { slug, file } ->
            parseRedirectsFile file
                |> DataSource.andThen
                    (\redirects ->
                        redirects
                            |> List.Extra.find (\redirect -> redirect.slug == slug)
                            |> Maybe.map
                                (\redirect -> DataSource.succeed <| ArticleLinkWithMetadata redirect)
                            |> Maybe.withDefault (DataSource.fail "Redirect not found")
                    )

        ArticleFile articleFile ->
            articleFileToFilePath articleFile
                |> DataSource.andThen (.path >> DataSource.File.onlyFrontmatter metadataDecoder)
                |> DataSource.map
                    (\metadata ->
                        ArticleFileWithMetadata
                            { slug = articleFile.slug
                            , metadata = metadata
                            }
                    )


tags : DataSource (List ( Tag, Int ))
tags =
    listWithMetadata
        |> DataSource.map
            (\articles ->
                articles
                    |> List.concatMap
                        (\article ->
                            case article of
                                ArticleFileWithMetadata { metadata } ->
                                    metadata.tags

                                ArticleLinkWithMetadata { metadata } ->
                                    metadata.tags
                        )
                    |> List.Extra.gatherEqualsBy Tag.toSlug
                    |> List.map (\( tag, copies ) -> ( tag, 1 + List.length copies ))
            )
        |> (-- FIXME
            -- if False then
            --     DataSource.distillSerializeCodec "tags" (Codec.list <| Codec.tuple Codec.string Codec.int)
            -- else
            identity
           )


articleFileToFilePath : { a | slug : String } -> DataSource { path : String, isMarkdown : Bool }
articleFileToFilePath { slug } =
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
