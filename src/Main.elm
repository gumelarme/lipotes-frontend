module Main exposing (..)

import Api
import Browser
import Html exposing (Html, button, div, option, select, text, textarea)
import Html.Attributes exposing (class, hidden, placeholder, value)
import Html.Events exposing (onClick, onInput)
import Http
import Json.Decode exposing (Decoder, field, list, map3, string)
import List


main =
    Browser.element { init = init, update = update, view = view, subscriptions = subscriptions }


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


type alias Model =
    { sampleTexts : Status (List SampleText)
    , inputText : String
    }


type Status a
    = Loading
    | Success a


init : () -> ( Model, Cmd Msg )
init _ =
    ( { sampleTexts = Loading
      , inputText = ""
      }
    , getSampleTexts
    )


type Msg
    = GotSampleTexts (Result Http.Error (List SampleText))
    | SetInputText String
    | SampleTextSelected String
    | TriggerAnalyze


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SampleTextSelected key ->
            let
                sampleText =
                    case model.sampleTexts of
                        Success texts ->
                            List.filter (\x -> x.id == key) texts

                        Loading ->
                            []

                text =
                    case List.head sampleText of
                        Just a ->
                            a.text

                        Nothing ->
                            ""
            in
            update (SetInputText text) model

        SetInputText text ->
            ( { model | inputText = text }, Cmd.none )

        GotSampleTexts result ->
            case result of
                Ok data ->
                    ( { model | sampleTexts = Success data }, Cmd.none )

                Err _ ->
                    ( { model | sampleTexts = Loading }, Cmd.none )

        TriggerAnalyze ->
            let
                _ =
                    Debug.log model.inputText ""
            in
            ( model, Cmd.none )


view : Model -> Html Msg
view model =
    div []
        [ viewHeader model
        , div [] [ viewTextArea model ]
        ]


viewHeader : Model -> Html Msg
viewHeader model =
    div [ class "flex p-2 px-4 bg-blue-800 text-white items-center" ]
        [ div [ class "grow text-2xl font-bold" ] [ text "Hanzi Memo" ]
        , div [ class "flex items-center gap-5" ]
            [ viewSelectTextPreset model
            , button [] [ text "About" ]
            ]
        ]


viewSelectTextPreset : Model -> Html Msg
viewSelectTextPreset model =
    select [ class "select select-md rounded-none", onInput SampleTextSelected ]
        (option [ hidden True ] [ text "Sample Text" ]
            :: (case model.sampleTexts of
                    Success res ->
                        List.map createOption res

                    Loading ->
                        []
               )
        )


viewTextArea : Model -> Html Msg
viewTextArea model =
    div [ class "flex relative h-40" ]
        [ textarea [ class "grow", placeholder "Type something" ] [ text model.inputText ]
        , button [ class "btn btn-sm btn-primary absolute right-4 bottom-4", onClick TriggerAnalyze ] [ text "Analyze" ]
        ]


createOption : SampleText -> Html Msg
createOption model =
    option [ value model.id ] [ text model.title ]


type alias SampleText =
    { id : String
    , title : String
    , text : String
    }


getSampleTexts : Cmd Msg
getSampleTexts =
    Http.get
        { url = Api.endpoint "/texts"
        , expect = Http.expectJson GotSampleTexts textDecoder
        }


dataDecoder : Decoder a -> Decoder (List a)
dataDecoder decoder =
    field "data" (list decoder)


textDecoder : Decoder (List SampleText)
textDecoder =
    dataDecoder
        (map3 SampleText
            (field "id" string)
            (field "title" string)
            (field "text" string)
        )
