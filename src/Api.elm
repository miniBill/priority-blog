module Api exposing (routes)

import ApiRoute
import Data.Article as Article
import DataSource exposing (DataSource)
import Dict exposing (Dict)
import Html as H exposing (Html)
import Route exposing (Route)


routes :
    DataSource (List Route)
    -> (Html Never -> String)
    -> List (ApiRoute.ApiRoute ApiRoute.Response)
routes _ htmlToString =
    [ ApiRoute.succeed (toRedirect htmlToString)
        |> ApiRoute.capture
        |> ApiRoute.buildTimeRoutes builder
    ]


toRedirect : (Html Never -> String) -> String -> DataSource ApiRoute.Response
toRedirect htmlToString slug =
    redirectsDict
        |> DataSource.andThen
            (\dict ->
                case Dict.get slug dict of
                    Just url ->
                        DataSource.succeed
                            { body = htmlToString <| H.text url
                            }

                    Nothing ->
                        DataSource.fail <| "Redirect not found for " ++ slug
            )


builder : (String -> List String) -> DataSource (List (List String))
builder toPath =
    redirectsDict
        |> DataSource.map (Dict.keys >> List.map toPath)


redirectsDict : DataSource (Dict String String)
redirectsDict =
    Article.listWithMetadata
        |> DataSource.map
            (\articles ->
                articles
                    |> List.filterMap
                        (\article ->
                            case article of
                                Article.ArticleLinkWithMetadata { slug, url } ->
                                    Just ( slug, url )

                                _ ->
                                    Nothing
                        )
                    |> Dict.fromList
            )
