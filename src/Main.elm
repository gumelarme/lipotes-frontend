module Main exposing (..)

import Browser
import Html exposing (Html, div, text)
import Html.Attributes exposing (class)


main: Program () Model Msg
main =
  Browser.sandbox { init = init, update = update, view = view }


type alias Model = String


init : Model
init = "Hello"

type Msg = NoOp


update : Msg -> Model -> Model
update _ model = model


view : Model -> Html Msg
view _ =
  div []
      [viewHeader]



viewHeader : Html Msg
viewHeader =
    div [class "flex p-2 bg-blue-800 text-white text-3xl font-bold"]
        [text "Hanzi Memo"]
