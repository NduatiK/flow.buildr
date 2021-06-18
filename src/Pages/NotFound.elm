module Pages.NotFound exposing (view)

import Element exposing (..)
import Gen.Route
import Request exposing (Request)
import View exposing (View)


view : View msg
view =
    { title = "Page not found."
    , body =
        [ Element.link [ centerX, centerY ]
            { url = Gen.Route.toHref Gen.Route.Flow_buildr
            , label = text "Lost? Click here to return to the app"
            }
        ]
    }
