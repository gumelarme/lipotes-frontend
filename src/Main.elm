module Main exposing (..)

import Api
import Browser
import List
import Html exposing (Html, div, text, select, option)
import Html.Attributes exposing (class)
import Html.Attributes exposing (hidden)
import Html exposing (button)
import Http
import Json.Decode exposing (Decoder, map3, field, string, list)



main =
  Browser.element { init = init, update = update, view = view, subscriptions = subscriptions }


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


type  Model
    = Loading
    | Success (List SampleText)



init : ()  -> (Model, Cmd Msg)
init _ =
    (Loading, getSampleText)


type Msg =
    GotData (Result Http.Error (List SampleText))

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        GotData result ->
            case result of
                Ok data ->
                    (Success data, Cmd.none)
                Err _ ->
                    (Loading, Cmd.none)



view : Model -> Html Msg
view model =
  div []
      [viewHeader model]



viewHeader : Model -> Html Msg
viewHeader model =
    div [ class "flex p-2 px-4 bg-blue-800 text-white items-center" ]
        [ div [ class "grow text-2xl font-bold"] [text "Hanzi Memo"]
        , div [ class "flex items-center gap-5"]
              [ viewSelectTextPreset model
              , button [] [text "About"]
              ]
        ]

type alias SampleText =
    { id: String
    , title: String
    , content: String
    }


viewSelectTextPreset: Model -> Html Msg
viewSelectTextPreset  model =
    select [ class "select select-md rounded-none" ]
        (option [ hidden True ] [text "Sample Text"]
        :: case model of
              Success res -> List.map createOption res
              Loading -> []
        )


createOption: SampleText -> Html Msg
createOption model =
    option [] [text model.title]


getSampleText : Cmd Msg
getSampleText =
    Http.get
        { url = Api.endpoint "/texts"
        , expect = Http.expectJson GotData textDecoder }


dataDecoder : Decoder a -> Decoder (List a)
dataDecoder decoder =
    (field "data" (list decoder))


textDecoder : Decoder (List SampleText)
textDecoder =
    dataDecoder
    (map3 SampleText
        (field "id" string)
        (field "title" string)
        (field "text" string))
