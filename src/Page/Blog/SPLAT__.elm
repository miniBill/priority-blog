module Page.Blog.SPLAT__ exposing (Data, Model, Msg, page)

import DataSource exposing (DataSource)
import DataSource.File
import DataSource.Glob as Glob
import Head
import Head.Seo as Seo
import OptimizedDecoder as Decode exposing (Decoder)
import Page exposing (Page, StaticPayload)
import Pages.PageUrl exposing (PageUrl)
import Pages.Url
import Shared
import View exposing (View)


type alias Model =
    ()


type alias Msg =
    Never


type alias RouteParams =
    { splat : List String
    }


page : Page RouteParams Data
page =
    Page.prerender
        { head = head
        , data = data
        , routes = routes
        }
        |> Page.buildNoState { view = view }


data : RouteParams -> DataSource Data
data { splat } =
    DataSource.File.bodyWithFrontmatter blogPostDecoder
        (String.join "/" ("articles" :: splat) ++ ".md")


blogPostDecoder : String -> Decoder Data
blogPostDecoder body =
    Decode.map2 (Data body)
        (Decode.field "title" Decode.string)
        (Decode.field "tags" tagsDecoder)


tagsDecoder : Decoder (List String)
tagsDecoder =
    Decode.map (String.split " ")
        Decode.string


head :
    StaticPayload Data RouteParams
    -> List Head.Tag
head static =
    Seo.summary
        { canonicalUrlOverride = Nothing
        , siteName = "elm-pages"
        , image =
            { url = Pages.Url.external "TODO"
            , alt = "elm-pages logo"
            , dimensions = Nothing
            , mimeType = Nothing
            }
        , description = "TODO"
        , locale = Nothing
        , title = "TODO title" -- metadata.title -- TODO
        }
        |> Seo.website


type alias Data =
    { body : String
    , title : String
    , tags : List String
    }


view :
    Maybe PageUrl
    -> Shared.Model
    -> StaticPayload Data RouteParams
    -> View Msg
view maybeUrl sharedModel static =
    { title = static.data.title
    , body = Err static.data.body
    }


routes : DataSource (List RouteParams)
routes =
    Glob.succeed (\slug -> { splat = slug })
        |> Glob.match (Glob.literal "articles/")
        |> Glob.capture Glob.recursiveWildcard
        |> Glob.match (Glob.literal ".md")
        |> Glob.toDataSource
