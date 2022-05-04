module Page.Article_ exposing (Data, Model, Msg, RouteParams, page)

import Data.Article as Article exposing (Article(..), ArticleMetadata, ArticleTime(..), ArticleWithMetadata(..))
import Data.Tag as Tag
import DataSource exposing (DataSource)
import Head
import Head.Seo as Seo
import Iso8601
import List.Extra
import Page exposing (Page, StaticPayload)
import Pages.PageUrl exposing (PageUrl)
import Pages.Url
import Path
import Serialize as Codec exposing (Codec)
import Shared
import Site
import Slug
import View exposing (ArticleData, Body(..), View)


type alias Msg =
    Never


type alias Model =
    ()


type alias RouteParams =
    { article : String
    }


type Data
    = DataArticle ArticleData
    | DataRedirect String
    | DataError


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
        (.articles >> List.map getRouteParamsWithMetadata)
        Article.listWithMetadata


getRouteParamsWithMetadata : ArticleWithMetadata -> RouteParams
getRouteParamsWithMetadata article =
    case article of
        ArticleFileWithMetadata { slug } ->
            { article = Slug.toString slug }

        ArticleLinkWithMetadata { slug } ->
            { article = Slug.toString slug }


getRouteParams : Article -> RouteParams
getRouteParams article =
    case article of
        ArticleFile { slug } ->
            { article = Slug.toString slug }

        ArticleLink { slug } ->
            { article = Slug.toString slug }


data : RouteParams -> DataSource Data
data params =
    let
        slugString =
            Slug.generate params.article
                |> Maybe.map Slug.toString
                |> Maybe.withDefault params.article
    in
    Article.list
        |> DataSource.andThen
            (\articles ->
                case
                    List.Extra.find
                        (\article -> (getRouteParams article).article == slugString)
                        articles
                of
                    Nothing ->
                        DataSource.fail <| "Article " ++ params.article ++ " not found "

                    Just article ->
                        case article of
                            ArticleFile f ->
                                Article.fetchArticleMetadata f
                                    |> DataSource.map
                                        (\maybeMetadata ->
                                            case maybeMetadata of
                                                Ok ( metadata, content ) ->
                                                    DataArticle
                                                        { content = content
                                                        , metadata = metadata
                                                        }

                                                Err _ ->
                                                    DataError
                                        )

                            ArticleLink redirect ->
                                Article.fetchRedirectMetadata redirect
                                    |> DataSource.map
                                        (\maybeMetadata ->
                                            case maybeMetadata of
                                                Ok { url } ->
                                                    DataRedirect url

                                                Err _ ->
                                                    DataError
                                        )
            )
        |> DataSource.distillSerializeCodec ("article-" ++ params.article) codec


codec : Codec () Data
codec =
    Codec.customType
        (\fhtml fredirect ferror value ->
            case value of
                DataArticle h ->
                    fhtml h

                DataRedirect r ->
                    fredirect r

                DataError ->
                    ferror
        )
        |> Codec.variant1 DataArticle htmlBodyCodec
        |> Codec.variant1 DataRedirect Codec.string
        |> Codec.variant0 DataError
        |> Codec.finishCustomType


htmlBodyCodec :
    Codec
        ()
        { content : String
        , metadata : ArticleMetadata
        }
htmlBodyCodec =
    Codec.record
        (\content metadata ->
            { content = content
            , metadata = metadata
            }
        )
        |> Codec.field .content Codec.string
        |> Codec.field .metadata Article.articleMetadataCodec
        |> Codec.finishRecord


head :
    StaticPayload Data RouteParams
    -> List Head.Tag
head static =
    case static.data of
        DataArticle { metadata } ->
            Seo.summary
                { canonicalUrlOverride = Nothing
                , siteName = Site.name
                , image =
                    case metadata.image of
                        Just imageUrl ->
                            { url =
                                if String.startsWith "http" imageUrl then
                                    Pages.Url.external imageUrl

                                else
                                    Pages.Url.fromPath <| Path.fromString imageUrl
                            , alt = "Picture for the article " ++ metadata.title
                            , dimensions = Nothing
                            , mimeType = Nothing
                            }

                        Nothing ->
                            Site.logo
                , description = Maybe.withDefault "" metadata.description
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

        DataRedirect url ->
            [ Head.metaRedirect <| Head.raw <| "0; url=" ++ url ]

        DataError ->
            Seo.summary
                { canonicalUrlOverride = Nothing
                , siteName = Site.name
                , image = Site.logo
                , description = ""
                , locale = Just "en"
                , title = ""
                }
                |> Seo.website


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
        DataArticle body ->
            { title = body.metadata.title ++ " (" ++ String.fromInt body.metadata.priority ++ ") - " ++ Site.name
            , body = ArticleBody body
            }

        DataRedirect url ->
            { title = "Redirecting to " ++ url
            , body = RedirectBody url
            }

        DataError ->
            { title = "Error"
            , body = RedirectBody "/"
            }
