module Site exposing (Data, config, description, logo, name)

import DataSource exposing (DataSource)
import Head
import Head.Seo as Seo
import LanguageTag
import LanguageTag.Language
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


data : DataSource Data
data =
    DataSource.succeed ()


head : Data -> List Head.Tag
head _ =
    [ Head.sitemapLink "/sitemap.xml"
    , Head.appleTouchIcon (Just 180) (Pages.Url.fromPath <| Path.fromString "apple-touch-icon.png")
    , Head.icon [ ( 32, 32 ) ] MimeType.Png <| Pages.Url.fromPath <| Path.fromString "favicon-32x32.png"
    , Head.icon [ ( 16, 16 ) ] MimeType.Png <| Pages.Url.fromPath <| Path.fromString "favicon-16x16.png"
    , LanguageTag.Language.en
        |> LanguageTag.build LanguageTag.emptySubtags
        |> Head.rootLanguage
    ]


manifest : Data -> Manifest.Config
manifest _ =
    let
        iconSmall : Manifest.Icon
        iconSmall =
            { src = Pages.Url.fromPath <| Path.fromString "android-chrome-192x192.png"
            , sizes = [ ( 192, 192 ) ]
            , purposes = [ Manifest.IconPurposeAny, Manifest.IconPurposeMaskable ]
            , mimeType = Just MimeType.Png
            }

        iconBig : Manifest.Icon
        iconBig =
            { src = Pages.Url.fromPath <| Path.fromString "android-chrome-512x512.png"
            , sizes = [ ( 512, 512 ) ]
            , purposes = [ Manifest.IconPurposeAny, Manifest.IconPurposeMaskable ]
            , mimeType = Just MimeType.Png
            }
    in
    Manifest.init
        { name = name
        , description = description
        , startUrl = Route.toPath Route.Index
        , icons = [ iconSmall, iconBig ]
        }
        |> Manifest.withDisplayMode Manifest.Standalone


logo : Seo.Image
logo =
    { url = Pages.Url.fromPath <| Path.fromString "android-chrome-512x512.png"
    , alt = "Incrium's logo"
    , dimensions = Just { width = 600, height = 600 }
    , mimeType = Just <| MimeType.toString <| MimeType.Image MimeType.Png
    }


name : String
name =
    "Incrium"


description : String
description =
    "An incremental blog"
