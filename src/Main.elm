module Main exposing (..)

import Browser
import List
import Html exposing (Html, div, text, select, option)
import Html.Attributes exposing (class)
import Html.Attributes exposing (hidden)
import Html exposing (button)
import Http
import Json.Decode exposing (Decoder, map, map3, field, string, list)
import Env



main =
  Browser.element { init = init, update = update, view = view, subscriptions = subscriptions }


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


type  Model
    = Loading
    | Success (Data SampleText)



init : ()  -> (Model, Cmd Msg)
init _ =
    (Loading, getSampleText)


type Msg =
    GotData (Result Http.Error (Data SampleText))

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
    case model of
        Success res ->
          select [ class "select select-md rounded-none" ]
              (option [ hidden True ] [text "Sample Text"]
              :: (List.map createOption res.data))
        Loading -> div [] []


createOption: SampleText -> Html Msg
createOption model =
    option [] [text model.title]


getSampleText : Cmd Msg
getSampleText =
    Http.get
        { url = Env.hzm_api_url ++ "/texts"
        , expect = Http.expectJson GotData (dataDecoder textDecoder) }


type alias Data a = {data: List a}

dataDecoder : Decoder a -> Decoder (Data a)
dataDecoder decoder =
    map Data
        (field "data" (list decoder))


textDecoder: Decoder SampleText
textDecoder =
    map3 SampleText
        (field "id" string)
        (field "title" string)
        (field "text" string)
