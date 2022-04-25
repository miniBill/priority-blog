module Page.Index exposing (Data, Item, LinkOrArticle(..), Model, Msg, RouteParams, articleToItem, linkOrArticleCodec, page, viewArticleList)

import Data.Article exposing (ArticleWithMetadata(..))
import Data.Tag as Tag exposing (Tag)
import DataSource exposing (DataSource)
import Head
import Head.Seo as Seo
import Html as H exposing (Html)
import Html.Attributes as HA
import Page exposing (Page, StaticPayload)
import Pages.PageUrl exposing (PageUrl)
import Route
import Serialize as Codec exposing (Codec)
import Shared
import Site
import Theme
import View exposing (Body(..), View)


type alias Model =
    ()


type alias Msg =
    Never


type alias RouteParams =
    {}


type alias Data =
    List Item


type alias Item =
    { priority : Int
    , page : LinkOrArticle
    , tags : List Tag
    , title : String
    }


type LinkOrArticle
    = Link String
    | Article { slug : String, description : Maybe String, image : Maybe String }


page : Page RouteParams Data
page =
    Page.single
        { head = head
        , data = data
        }
        |> Page.buildNoState { view = view }


view :
    Maybe PageUrl
    -> Shared.Model
    -> StaticPayload Data RouteParams
    -> View Msg
view _ _ static =
    { title = ""
    , body = viewArticleList static.data
    }


viewArticleList : List Item -> Body Msg
viewArticleList list =
    list
        |> List.sortBy .priority
        |> List.map viewLink
        |> HtmlBody


viewLink : Item -> Html Msg
viewLink item =
    let
        link description image attrs =
            let
                tags =
                    List.map Shared.viewTag item.tags
                        |> List.intersperse (H.text ", ")
                        |> H.div
                            [ HA.style "display" "inline-block"
                            , HA.style "font-size" "0.8rem"
                            ]

                main =
                    H.div [ HA.style "flex-grow" "1" ]
                        ([ Theme.priorityBadge item.priority
                         , H.text " "
                         , H.a
                            attrs
                            [ H.text item.title ]
                         , H.text " - "
                         , tags
                         ]
                            ++ (case description of
                                    Nothing ->
                                        []

                                    Just d ->
                                        [ H.br [] []
                                        , H.text d
                                        ]
                               )
                        )
            in
            H.div [ HA.style "display" "flex" ]
                (case image of
                    Nothing ->
                        [ main ]

                    Just src ->
                        [ main, H.img [ HA.src src, HA.style "width" "15vw" ] [] ]
                )
    in
    case item.page of
        Article { slug, description, image } ->
            Route.toLink (link description image) (Route.Article_ { article = slug })

        Link url ->
            link Nothing Nothing [ HA.href url ]


data : DataSource Data
data =
    Data.Article.listWithMetadata
        |> DataSource.map (List.filterMap articleToItem)
        |> DataSource.distillSerializeCodec "index" codec


codec : Codec () (List Item)
codec =
    Codec.list
        (Codec.record Item
            |> Codec.field .priority Codec.int
            |> Codec.field .page linkOrArticleCodec
            |> Codec.field .tags Tag.listCodec
            |> Codec.field .title Codec.string
            |> Codec.finishRecord
        )


linkOrArticleCodec : Codec () LinkOrArticle
linkOrArticleCodec =
    Codec.customType
        (\farticle flink value ->
            case value of
                Article { slug, description, image } ->
                    farticle slug description image

                Link url ->
                    flink url
        )
        |> Codec.variant3
            (\slug description image ->
                Article
                    { slug = slug
                    , description = description
                    , image = image
                    }
            )
            Codec.string
            (Codec.maybe Codec.string)
            (Codec.maybe Codec.string)
        |> Codec.variant1 Link Codec.string
        |> Codec.finishCustomType


articleToItem : ArticleWithMetadata -> Maybe Item
articleToItem article =
    case article of
        ArticleFileWithMetadata { slug, metadata } ->
            Just
                { title = metadata.title
                , tags = metadata.tags
                , priority = metadata.priority
                , page =
                    Article
                        { slug = slug
                        , description = metadata.description
                        , image = metadata.image
                        }
                }

        ArticleLinkWithMetadata { url, metadata } ->
            Maybe.map2
                (\title priority ->
                    { title = title
                    , tags = metadata.tags
                    , priority = priority
                    , page = Link url
                    }
                )
                metadata.title
                metadata.priority


head :
    StaticPayload Data RouteParams
    -> List Head.Tag
head _ =
    Seo.summary
        { canonicalUrlOverride = Nothing
        , siteName = Site.name
        , image = Site.logo
        , description = Site.description
        , locale = Nothing
        , title = Site.name
        }
        |> Seo.website
