module Page.Article_ exposing (Data, Model, Msg, RouteParams, page)

import Data.Article as Article exposing (Article(..), ArticleMetadata, ArticleTime(..))
import Data.Tag as Tag
import DataSource exposing (DataSource)
import DataSource.File
import Head
import Head.Seo as Seo
import Iso8601
import List.Extra
import OptimizedDecoder as Decode
import Page exposing (Page, StaticPayload)
import Pages.PageUrl exposing (PageUrl)
import Pages.Url
import Serialize as Codec exposing (Codec)
import Shared
import Site
import View exposing (ArticleData, Body(..), View)


type alias Msg =
    Never


type alias Model =
    ()


type alias RouteParams =
    { article : String
    }


type Data
    = HtmlBody ArticleData
    | Redirect String


page : Page RouteParams Data
page =
    Page.prerender
        { head = head
        , data = data
        , routes = routes
        }
        |> Page.buildNoState { view = view }


routes : DataSource (List RouteParams)
routes =
    DataSource.map
        (List.map getRouteParams)
        Article.list


getRouteParams : Article -> RouteParams
getRouteParams article =
    case article of
        ArticleFile { slug } ->
            { article = slug }

        ArticleLink { slug } ->
            { article = slug }


data : RouteParams -> DataSource Data
data params =
    Article.list
        |> DataSource.andThen
            (\articles ->
                case
                    List.Extra.find
                        (\article -> getRouteParams article == params)
                        articles
                of
                    Nothing ->
                        DataSource.fail <| "Article " ++ params.article ++ " not found "

                    Just article ->
                        case article of
                            ArticleFile f ->
                                let
                                    path =
                                        Article.getArticlePath f
                                in
                                DataSource.File.bodyWithFrontmatter
                                    (\content ->
                                        Decode.map
                                            (\metadata ->
                                                HtmlBody
                                                    { content = content
                                                    , isMarkdown = f.isMarkdown
                                                    , metadata = metadata
                                                    }
                                            )
                                            Article.metadataDecoder
                                    )
                                    path

                            ArticleLink redirect ->
                                Article.fetchRedirectMetadata redirect
                                    |> DataSource.map (\{ url } -> Redirect url)
            )
        |> DataSource.distillSerializeCodec ("article-" ++ params.article) codec


codec : Codec () Data
codec =
    Codec.customType
        (\fhtml fredirect value ->
            case value of
                HtmlBody h ->
                    fhtml h

                Redirect r ->
                    fredirect r
        )
        |> Codec.variant1 HtmlBody htmlBodyCodec
        |> Codec.variant1 Redirect Codec.string
        |> Codec.finishCustomType


htmlBodyCodec :
    Codec
        ()
        { content : String
        , isMarkdown : Bool
        , metadata : ArticleMetadata
        }
htmlBodyCodec =
    Codec.record
        (\content isMarkdown metadata ->
            { content = content
            , isMarkdown = isMarkdown
            , metadata = metadata
            }
        )
        |> Codec.field .content Codec.string
        |> Codec.field .isMarkdown Codec.bool
        |> Codec.field .metadata Article.articleMetadataCodec
        |> Codec.finishRecord


head :
    StaticPayload Data RouteParams
    -> List Head.Tag
head static =
    case static.data of
        HtmlBody { metadata } ->
            Seo.summary
                { canonicalUrlOverride = Nothing
                , siteName = "Incrium"
                , image =
                    extractImage static
                        |> Maybe.withDefault Site.logo
                , description = ""
                , locale = Just "en"
                , title = metadata.title
                }
                |> Seo.article
                    { tags = List.map Tag.name metadata.tags
                    , publishedTime = Maybe.andThen toSeoTime metadata.datePublished
                    , modifiedTime = Maybe.andThen toSeoTime metadata.dateUpdated
                    , expirationTime = Nothing
                    , section = Nothing
                    }

        Redirect url ->
            [ Head.metaRedirect <| Head.raw <| "0; url=" ++ url ]


extractImage : StaticPayload Data RouteParams -> Maybe Seo.Image
extractImage _ =
    -- TODO
    Nothing


toSeoTime : ArticleTime -> Maybe String
toSeoTime time =
    case time of
        Iso8601 t ->
            Just <| Iso8601.fromTime t

        Date _ ->
            Nothing


view :
    Maybe PageUrl
    -> Shared.Model
    -> StaticPayload Data RouteParams
    -> View Msg
view _ _ static =
    case static.data of
        HtmlBody body ->
            { title = body.metadata.title ++ " (" ++ String.fromInt body.metadata.priority ++ ") - " ++ Site.name
            , body = ArticleBody body
            }

        Redirect url ->
            { title = "Redirecting to " ++ url
            , body = RedirectBody url
            }
