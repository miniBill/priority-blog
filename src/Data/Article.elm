module Data.Article exposing (Article(..), ArticleMetadata, ArticleTime(..), ArticleWithMetadata(..), LinkMetadata, Redirect, articleMetadataCodec, codec, fetchArticleMetadata, fetchRedirectMetadata, getArticlePath, list, listWithMetadata, slugCodec, tags)

import Data.Tag as Tag exposing (Tag)
import DataSource exposing (DataSource)
import DataSource.File
import DataSource.Glob as Glob
import Date
import Iso8601
import List.Extra
import Markdown.Block as Block
import Markdown.Parser
import Parser exposing ((|.), (|=), Parser)
import Serialize as Codec exposing (Codec)
import Slug exposing (Slug)
import String.Extra
import Time


type Article
    = ArticleFile { slug : Slug, file : String, isMarkdown : Bool }
    | ArticleLink { slug : Slug, file : String }


type ArticleWithMetadata
    = ArticleFileWithMetadata
        { slug : Slug
        , metadata : ArticleMetadata
        }
    | ArticleLinkWithMetadata Redirect


type alias Redirect =
    { slug : Slug
    , url : String
    , metadata : LinkMetadata
    }


type alias ArticleMetadata =
    { title : String
    , description : Maybe String
    , image : Maybe String
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
                |> Codec.field .slug slugCodec
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
                |> Codec.field .slug slugCodec
                |> Codec.field .url Codec.string
                |> Codec.field .metadata linkMetadataCodec
                |> Codec.finishRecord
            )
        |> Codec.finishCustomType


slugCodec : Codec () Slug
slugCodec =
    Codec.mapValid
        (\s ->
            case Slug.parse s of
                Just slug ->
                    Ok slug

                Nothing ->
                    Err ()
        )
        Slug.toString
        Codec.string


articleMetadataCodec : Codec () ArticleMetadata
articleMetadataCodec =
    Codec.record ArticleMetadata
        |> Codec.field .title Codec.string
        |> Codec.field .description (Codec.maybe Codec.string)
        |> Codec.field .image (Codec.maybe Codec.string)
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
        parseSlugs lst =
            lst
                |> List.map parseSlug
                |> DataSource.combine

        parseSlug { file, isMarkdown } =
            case Slug.generate file of
                Just slug ->
                    DataSource.succeed <|
                        ArticleFile
                            { file = file
                            , slug = slug
                            , isMarkdown = isMarkdown
                            }

                Nothing ->
                    DataSource.fail <| "Could not generate slug for article " ++ file

        markdowns =
            Glob.succeed (\file -> { file = file, isMarkdown = True })
                |> Glob.match (Glob.literal "articles/")
                |> Glob.capture Glob.wildcard
                |> Glob.match (Glob.literal ".md")
                |> Glob.toDataSource
                |> DataSource.andThen parseSlugs

        htmls =
            Glob.succeed (\file -> { file = file, isMarkdown = False })
                |> Glob.match (Glob.literal "articles/")
                |> Glob.capture Glob.wildcard
                |> Glob.match (Glob.literal ".html")
                |> Glob.toDataSource
                |> DataSource.andThen parseSlugs

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
                    |> String.split "\n"
                    |> List.filterMap parseRedirectLine
            )


parseRedirectLine : String -> Maybe Redirect
parseRedirectLine line =
    line
        |> Parser.run redirectLineParser
        |> Result.toMaybe


redirectLineParser : Parser Redirect
redirectLineParser =
    Parser.succeed
        (\slug url metadata ->
            { slug = slug
            , url =
                if String.contains ":" url then
                    String.trim url

                else
                    "https://" ++ String.trim url
            , metadata = metadata
            }
        )
        |= Parser.andThen linkSlugParser (Parser.getChompedString (Parser.chompUntil ":"))
        |. Parser.symbol ":"
        |. Parser.spaces
        |= Parser.getChompedString (Parser.chompUntilEndOr " ")
        |. Parser.spaces
        |= metadataParser


linkSlugParser : String -> Parser Slug
linkSlugParser slug =
    case Slug.generate slug of
        Just s ->
            Parser.succeed s

        Nothing ->
            Parser.problem <| "Cannot generate slug for link " ++ slug


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
                                                    |> String.split ","
                                                    |> List.filterMap Tag.fromString
                                        }

                                    "datePublished" ->
                                        { acc | datePublished = timeParser v }

                                    "dateUpdated" ->
                                        { acc | dateUpdated = timeParser v }

                                    _ ->
                                        acc
                            )
                            empty
                )
        , Parser.succeed empty
        ]


listWithMetadata :
    DataSource
        { articles : List ArticleWithMetadata
        , errors : List { file : String, error : String }
        }
listWithMetadata =
    list
        |> DataSource.andThen
            (\articles ->
                articles
                    |> List.map fetchMetadata
                    |> DataSource.combine
                    |> DataSource.map
                        (List.foldl
                            (\e a ->
                                case e of
                                    Ok c ->
                                        { a | articles = c :: a.articles }

                                    Err err ->
                                        { a | errors = err :: a.errors }
                            )
                            { articles = [], errors = [] }
                        )
            )


fetchMetadata : Article -> DataSource (Result { file : String, error : String } ArticleWithMetadata)
fetchMetadata article =
    case article of
        ArticleLink data ->
            fetchRedirectMetadata data
                |> DataSource.map (Result.map ArticleLinkWithMetadata)

        ArticleFile articleFile ->
            fetchArticleMetadata articleFile
                |> DataSource.map
                    (Result.map <|
                        \( metadata, _ ) ->
                            ArticleFileWithMetadata
                                { slug = articleFile.slug
                                , metadata = metadata
                                }
                    )


fetchArticleMetadata :
    { a | file : String, isMarkdown : Bool }
    -> DataSource (Result { file : String, error : String } ( ArticleMetadata, String ))
fetchArticleMetadata articleFile =
    let
        path =
            getArticlePath articleFile
    in
    path
        |> DataSource.File.rawFile
        |> DataSource.map
            (\file ->
                case String.split "---" file of
                    before :: between :: after ->
                        case String.trim before of
                            "" ->
                                case parseMetadata between of
                                    Err e ->
                                        Err <| "error parsing metadata: " ++ e

                                    Ok metadata ->
                                        let
                                            rebuilt =
                                                String.join "---" after

                                            parsed =
                                                Markdown.Parser.parse rebuilt
                                                    |> Result.withDefault []

                                            filledMetadata =
                                                if articleFile.isMarkdown then
                                                    { metadata
                                                        | description = markdownToDescription parsed
                                                        , image = markdownToImage parsed
                                                    }

                                                else
                                                    metadata
                                        in
                                        Ok
                                            ( filledMetadata
                                            , rebuilt
                                            )

                            nonempty ->
                                Err <| "Expected nothing before the first '---', but instead I found " ++ nonempty

                    _ ->
                        Err <| "Missing header"
            )
        |> DataSource.map (Result.mapError (\error -> { file = articleFile.file, error = error }))


parseMetadata : String -> Result String ArticleMetadata
parseMetadata header =
    header
        |> String.split "\n"
        |> List.foldl (\line -> Result.andThen (parseLine line))
            (Ok
                { title = Nothing
                , description = Nothing
                , image = Nothing
                , tags = []
                , priority = Nothing
                , datePublished = Nothing
                , dateUpdated = Nothing
                }
            )
        |> Result.andThen
            (\found ->
                case ( found.title, found.priority ) of
                    ( Nothing, _ ) ->
                        Err "Missing title"

                    ( _, Nothing ) ->
                        Err "Missing priotity"

                    ( Just title, Just priority ) ->
                        Ok
                            { title = title
                            , description = found.description
                            , image = found.image
                            , tags = found.tags
                            , priority = priority
                            , datePublished = found.datePublished
                            , dateUpdated = found.dateUpdated
                            }
            )


type alias ParsingMetadata =
    { title : Maybe String
    , description : Maybe String
    , image : Maybe String
    , tags : List Tag
    , priority : Maybe Int
    , datePublished : Maybe ArticleTime
    , dateUpdated : Maybe ArticleTime
    }


parseLine : String -> ParsingMetadata -> Result String ParsingMetadata
parseLine line acc =
    let
        cleaned =
            String.Extra.clean line
    in
    if String.isEmpty cleaned then
        Ok acc

    else
        case String.indexes ":" cleaned of
            splitAt :: _ ->
                let
                    before =
                        String.trim <| String.left splitAt cleaned

                    after =
                        String.trim <| String.dropLeft (splitAt + 1) cleaned
                in
                if String.isEmpty after then
                    Ok acc

                else
                    case before of
                        "title" ->
                            Ok { acc | title = Just after }

                        "tags" ->
                            Ok { acc | tags = parseTags after }

                        "priority" ->
                            case String.toInt after of
                                Just priority ->
                                    Ok { acc | priority = Just priority }

                                Nothing ->
                                    Err "Error parsing priority as an integer"

                        "date-published" ->
                            case timeParser after of
                                Just time ->
                                    Ok { acc | datePublished = Just time }

                                Nothing ->
                                    Err "Error parsing date-published"

                        "date-updated" ->
                            case timeParser after of
                                Just time ->
                                    Ok { acc | dateUpdated = Just time }

                                Nothing ->
                                    Err "Error parsing date-updated"

                        unexpected ->
                            Err <| "Unexpected property in header: " ++ unexpected

            [] ->
                Err <| "Unexpected line in header: " ++ cleaned


fetchRedirectMetadata :
    { file : String, slug : Slug }
    -> DataSource (Result { file : String, error : String } { slug : Slug, url : String, metadata : LinkMetadata })
fetchRedirectMetadata { file, slug } =
    parseRedirectsFile file
        |> DataSource.map
            (\redirects ->
                redirects
                    |> List.Extra.find (\redirect -> redirect.slug == slug)
                    |> Maybe.map
                        (\redirect -> Ok redirect)
                    |> Maybe.withDefault (Err { file = file, error = "Redirect not found" })
            )


tags : DataSource (List ( Tag, Int ))
tags =
    listWithMetadata
        |> DataSource.map
            (\{ articles } ->
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
        |> DataSource.distillSerializeCodec "tags" tagsCodec


tagsCodec : Codec () (List ( Tag, Int ))
tagsCodec =
    Codec.record
        (\tgs counts ->
            List.map2 Tuple.pair tgs counts
        )
        |> Codec.field (List.map Tuple.first) Tag.listCodec
        |> Codec.field (List.map Tuple.second) (Codec.list Codec.int)
        |> Codec.finishRecord


getArticlePath : { a | file : String, isMarkdown : Bool } -> String
getArticlePath { file, isMarkdown } =
    if isMarkdown then
        "articles/" ++ file ++ ".md"

    else
        "articles/" ++ file ++ ".html"


timeParser : String -> Maybe ArticleTime
timeParser raw =
    case Iso8601.toTime raw of
        Ok posix ->
            Just <| Iso8601 posix

        Err _ ->
            case List.map String.toInt <| String.split "/" (String.trim raw) of
                [ Just m, Just d, Just y ] ->
                    Just <| Date <| Date.fromCalendarDate y (Date.numberToMonth m) d

                _ ->
                    Nothing


parseTags : String -> List Tag
parseTags input =
    input
        |> String.split ","
        |> List.filterMap Tag.fromString



-- Extractors --


markdownToDescription : List Block.Block -> Maybe String
markdownToDescription blocks =
    let
        go budget bs =
            if budget < 0 then
                ""

            else
                case bs of
                    [] ->
                        ""

                    (Block.Paragraph inlines) :: t ->
                        let
                            extracted =
                                String.Extra.clean <| Block.extractInlineText inlines
                        in
                        if String.isEmpty extracted then
                            go budget t

                        else
                            extracted ++ " " ++ go (budget - 1 - String.length extracted) t

                    _ :: t ->
                        go budget t

        raw =
            go 200 blocks
    in
    if String.isEmpty raw then
        Nothing

    else
        Just (String.Extra.softEllipsis 200 raw)


markdownToImage : List Block.Block -> Maybe String
markdownToImage =
    List.Extra.findMap
        (\block ->
            case block of
                Block.Paragraph inlines ->
                    inlinesToImage inlines

                _ ->
                    Nothing
        )


inlinesToImage : List Block.Inline -> Maybe String
inlinesToImage =
    List.Extra.findMap
        (\inline ->
            case inline of
                Block.Image url _ _ ->
                    Just url

                _ ->
                    Nothing
        )
