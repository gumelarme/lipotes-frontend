port module Main exposing (..)

import Api
import Browser
import Html exposing (Html, button, div, form, h2, input, label, node, option, p, select, text, textarea)
import Html.Attributes exposing (checked, class, classList, hidden, id, method, placeholder, type_, value)
import Html.Events exposing (onCheck, onClick, onInput)
import Http
import Json.Decode exposing (Decoder, field, list, map3, map4, string)
import List
import List.Extra


main : Program () Model Msg
main =
    Browser.element { init = init, update = update, view = view, subscriptions = subscriptions }


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


type alias Model =
    { inputText : String
    , visibility : Visibility
    , sampleTexts : Status (List SampleText)
    , collections : Status (List ( Collection, Bool ))
    }


type Status a
    = Loading
    | Success a


withDefault : Status a -> a -> a
withDefault unknownData defaultValue =
    case unknownData of
        Success a ->
            a

        _ ->
            defaultValue


init : () -> ( Model, Cmd Msg )
init _ =
    ( { sampleTexts = Loading
      , collections = Loading
      , inputText = ""
      , visibility = Smart
      }
    , Cmd.batch [ getSampleTexts, getCollections ]
    )


type ModalId
    = ModalBlacklist
    | ModalAbout


modalIdStr : ModalId -> String
modalIdStr id =
    case id of
        ModalBlacklist ->
            "blacklist-modal"

        ModalAbout ->
            "about-modal"


type Msg
    = GotSampleTexts (Result Http.Error (List SampleText))
    | GotCollections (Result Http.Error (List Collection))
    | SetInputText String
    | SampleTextSelected String
    | CollectionSelectionChanged String Bool
    | TriggerAnalyze
    | VisibilityChanged Visibility
    | ToggleModal ModalId


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

        VisibilityChanged visibility ->
            ( { model | visibility = visibility }, Cmd.none )

        ToggleModal modalId ->
            ( model, toggleDialog (modalIdStr modalId) )

        GotCollections result ->
            case result of
                Ok data ->
                    ( { model | collections = Success (List.map (\x -> ( x, False )) data) }, Cmd.none )

                Err _ ->
                    ( { model | collections = Loading }, Cmd.none )

        CollectionSelectionChanged collId isChecked ->
            let
                collections =
                    withDefault model.collections []

                toggleStatus pair =
                    if (Tuple.first pair).id == collId then
                        ( Tuple.first pair, isChecked )

                    else
                        pair
            in
            ( { model | collections = Success (List.map toggleStatus collections) }
            , Cmd.none
            )


view : Model -> Html Msg
view model =
    div []
        [ viewHeader model
        , div [] [ viewMenu model, viewTextArea model ]
        , modal model ModalBlacklist "Blacklist" viewModalBlacklist
        , modal model ModalAbout "About" viewModalAbout
        ]


port toggleDialog : String -> Cmd msg


dialog : String -> List (Html.Attribute msg) -> List (Html msg) -> Html msg
dialog elementId attr content =
    node "dialog" (id elementId :: attr) content


modal : Model -> ModalId -> String -> (Model -> Html Msg) -> Html Msg
modal model modalId title content =
    dialog (modalIdStr modalId)
        [ class "modal" ]
        [ form [ method "dialog", class "modal-backdrop" ]
            [ button [] [ text "close" ] ]
        , div
            [ class "modal-box" ]
            [ modalControl model modalId title content ]
        ]


modalControl : Model -> ModalId -> String -> (Model -> Html Msg) -> Html Msg
modalControl model modalId title content =
    div [ class "flex flex-col gap-4" ]
        [ div []
            [ h2 [ class "grow font-bold text-2xl" ] [ text title ]
            , button
                [ class "btn btn-md btn-circle btn-ghost absolute top-3 right-3"
                , onClick (ToggleModal modalId)
                ]
                -- Change to icon
                [ text "X" ]
            ]
        , content model
        ]


viewModalBlacklist : Model -> Html Msg
viewModalBlacklist model =
    div [ class "flex flex-col gap-4" ]
        [ div [ class "h-[74vh] overflow-y-scroll" ]
            [ viewCollectionList model ]
        , div [ class "flex gap-2" ]
            [ button [ class "btn btn-primary", onClick (ToggleModal ModalBlacklist) ]
                [ text "Ok" ]
            , button [ class "btn btn-error", onClick (ToggleModal ModalBlacklist) ]
                [ text "Cancel" ]
            ]
        ]


viewCollectionList : Model -> Html Msg
viewCollectionList model =
    let
        collections =
            withDefault model.collections []

        divider =
            List.repeat (List.length collections) (div [ class "h-1.5" ] [ text "" ])

        createCard coll =
            viewCollectionItem (Tuple.first coll) (Tuple.second coll)

        collectionCards =
            List.map createCard collections

        combined =
            List.Extra.interweave collectionCards divider
    in
    div [] (List.take (List.length combined - 1) combined)


viewCollectionItem : Collection -> Bool -> Html Msg
viewCollectionItem collection state =
    div [ class "flex items-center bg-gray-800 p-4 hover:bg-gray-900 rounded" ]
        [ div [ class "grow flex flex-col" ]
            [ h2 [ class "text-2xl" ] [ text collection.name ]
            , p [ class "" ] [ text "description" ]
            ]
        , input
            [ type_ "checkbox"
            , class "toggle"
            , checked state
            , onCheck (CollectionSelectionChanged collection.id)
            ]
            []
        ]


viewModalAbout : a -> Html msg
viewModalAbout _ =
    div []
        [ text "About us" ]


viewHeader : Model -> Html Msg
viewHeader model =
    div [ class "flex p-2 px-4 bg-blue-800 text-white items-center" ]
        [ div [ class "grow text-2xl font-bold" ] [ text "Hanzi Memo" ]
        , div [ class "flex items-center gap-5" ]
            [ viewSelectTextPreset model
            , button [ onClick (ToggleModal ModalAbout) ] [ text "About" ]
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
        [ textarea
            [ class "grow p-2 text-xl"
            , placeholder "Type something..."
            , value model.inputText
            , onInput SetInputText
            ]
            []
        , button [ class "btn btn-sm btn-primary absolute right-4 bottom-4", onClick TriggerAnalyze ] [ text "Analyze" ]
        ]


viewMenu : Model -> Html Msg
viewMenu model =
    div [ class "flex font-bold text-xl h-12 items-stretch bg-blue-800" ]
        [ viewBlacklistButton
        , div [ class "grow" ] []
        , viewVisibilityMenu model
        ]


viewBlacklistButton : Html Msg
viewBlacklistButton =
    button [ class "px-2 bg-white text-black", onClick (ToggleModal ModalBlacklist) ]
        [ text "Blacklist" ]


type Visibility
    = Smart
    | HideAll
    | ShowAll


visibilityOptions : List ( Visibility, String )
visibilityOptions =
    [ ( HideAll, "Hide All" )
    , ( Smart, "Smart" )
    , ( ShowAll, "Show All" )
    ]


viewVisibilityMenu : Model -> Html Msg
viewVisibilityMenu model =
    div [ class "flex h-full" ]
        (List.map (\x -> menuRadio x (model.visibility == Tuple.first x)) visibilityOptions)


menuRadio : ( Visibility, String ) -> Bool -> Html Msg
menuRadio menu isChecked =
    label
        [ class "flex items-center px-2 text-white font-bold hover:cursor-pointer"
        , classList [ ( "bg-white text-black", isChecked ) ]
        ]
        [ input
            [ type_ "radio"
            , checked isChecked
            , onCheck (\_ -> VisibilityChanged (Tuple.first menu))
            , hidden True
            ]
            []
        , text (Tuple.second menu)
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


type alias Lexeme =
    { id : String
    , zh_sc : String
    , zh_tc : String
    , pinyin : String
    }


lexemeDecoder : Decoder Lexeme
lexemeDecoder =
    map4 Lexeme
        (field "id" string)
        (field "zh_sc" string)
        (field "zh_tc" string)
        (field "pinyin" string)


type alias Collection =
    { id : String
    , name : String
    , preview : List Lexeme
    }


collectionDecoder : Decoder (List Collection)
collectionDecoder =
    dataDecoder
        (map3 Collection
            (field "id" string)
            (field "name" string)
            (field "preview" (list lexemeDecoder))
        )


getCollections : Cmd Msg
getCollections =
    Http.get
        { url = Api.endpoint "/collections"
        , expect = Http.expectJson GotCollections collectionDecoder
        }
