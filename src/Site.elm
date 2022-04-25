module Site exposing (Data, config, description, logo, name)

import DataSource
import Head
import Head.Seo as Seo
import MimeType
import Pages.Manifest as Manifest
import Pages.Url
import Path
import Route
import SiteConfig exposing (SiteConfig)


type alias Data =
    ()


config : SiteConfig Data
config =
    { data = data
    , canonicalUrl = "https://incrium.com"
    , manifest = manifest
    , head = head
    }


data : DataSource.DataSource Data
data =
    DataSource.succeed ()


head : Data -> List Head.Tag
head _ =
    [ Head.sitemapLink "/sitemap.xml"
    ]


manifest : Data -> Manifest.Config
manifest _ =
    let
        icon : Manifest.Icon
        icon =
            { src = logo.url
            , sizes = [ ( 100, 100 ) ]
            , purposes = [ Manifest.IconPurposeAny ]
            , mimeType = Just svgMime
            }
    in
    Manifest.init
        { name = name
        , description = description
        , startUrl = Route.toPath Route.Index
        , icons = [ icon ]
        }


svgMime : MimeType.MimeImage
svgMime =
    MimeType.OtherImage "svg+xml"


logo : Seo.Image
logo =
    { url = Pages.Url.fromPath <| Path.fromString "logo.svg"
    , alt = "Incrium's logo"
    , dimensions = Just { width = 200, height = 200 }
    , mimeType = Just <| MimeType.toString <| MimeType.Image svgMime
    }


name : String
name =
    "Incrium"


description : String
description =
    "An incremental blog"
