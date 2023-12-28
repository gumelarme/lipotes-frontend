module Modal exposing (..)

import Html exposing (Html, button, div, form, h2, node, text)
import Html.Attributes exposing (class, id, method)
import Html.Events exposing (onClick)


dialog : String -> List (Html.Attribute msg) -> List (Html msg) -> Html msg
dialog elementId attr content =
    node "dialog" (id elementId :: attr) content


modal : a -> String -> String -> (a -> Html msg) -> msg -> Html msg
modal model modalId title content onCancel =
    dialog modalId
        [ class "modal" ]
        [ form [ method "dialog", class "modal-backdrop" ]
            [ button [ onClick onCancel ] [ text "close" ] ]
        , div
            [ class "modal-box" ]
            [ modalControl model title content onCancel ]
        ]


modalControl : a -> String -> (a -> Html msg) -> msg -> Html msg
modalControl model title content onCancel =
    div [ class "flex flex-col gap-4" ]
        [ div []
            [ h2 [ class "grow font-bold text-xl md:text-2xl" ] [ text title ]
            , button
                [ class "btn btn-md btn-circle btn-ghost absolute top-3 right-3"
                , onClick onCancel
                ]
                -- Change to icon
                [ text "X" ]
            ]
        , content model
        ]
