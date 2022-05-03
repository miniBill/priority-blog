module Page.BuildErrors exposing (Data, Model, Msg, page)

import Data.Article as Article
import DataSource exposing (DataSource)
import Head
import Head.Seo as Seo
import Html as H
import Page exposing (Page, StaticPayload)
import Pages.PageUrl exposing (PageUrl)
import Serialize as Codec exposing (Codec)
import Shared
import Site
import View exposing (Body(..), View)


type alias Model =
    ()


type alias Msg =
    Never


type alias RouteParams =
    {}


page : Page RouteParams Data
page =
    Page.single
        { head = head
        , data = data
        }
        |> Page.buildNoState { view = view }


type alias Data =
    List { file : String, error : String }


data : DataSource Data
data =
    Article.listWithMetadata
        |> DataSource.map .errors
        |> DataSource.distillSerializeCodec "build-errors" errorsCodec


errorsCodec : Codec () Data
errorsCodec =
    Codec.list
        (Codec.record (\file error -> { file = file, error = error })
            |> Codec.field .file Codec.string
            |> Codec.field .error Codec.string
            |> Codec.finishRecord
        )


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


view :
    Maybe PageUrl
    -> Shared.Model
    -> StaticPayload Data RouteParams
    -> View Msg
view _ _ static =
    { title = "Errors"
    , body =
        HtmlBody
            [ static.data
                |> List.concatMap
                    (\{ file, error } ->
                        [ H.dt [] [ H.text file ]
                        , H.dd [] [ H.text error ]
                        ]
                    )
                |> H.dl []
            ]
    }
