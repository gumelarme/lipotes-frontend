module Main exposing (..)

import Api
import Browser
import Data
import Html exposing (Html, button, div, h2, input, label, option, p, select, span, text, textarea)
import Html.Attributes exposing (checked, class, classList, hidden, id, placeholder, type_, value)
import Html.Events exposing (onCheck, onClick, onInput)
import Http
import Json.Decode as D
import Json.Encode as E
import List
import Modal
import Port
import Set exposing (Set)


main : Program E.Value Model Msg
main =
    Browser.element { init = init, update = updateWithStore, view = view, subscriptions = subscriptions }


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


type alias Model =
    { inputText : String
    , visibility : Visibility
    , sampleTexts : Status (List Data.SampleText)
    , collections : Status (List Data.Collection)
    , selectedCollections : Set String
    , tempSelectedCollections : Set String
    }


type alias ErrorMessage =
    { message : String
    , resolution : String
    }


friendlyHttpError : Http.Error -> ErrorMessage
friendlyHttpError error =
    case error of
        Http.NetworkError ->
            { message = "You seems to be offline."
            , resolution = "Check your internet connection and try again."
            }

        Http.Timeout ->
            { message = "The server took too long to respond."
            , resolution = "Please try again in a few moments."
            }

        _ ->
            { message = "Somethings wrong."
            , resolution = "Try reloading the page."
            }


type Status a
    = Loading
    | Failed ErrorMessage
    | Success a


withDefault : a -> Status a -> a
withDefault defaultValue status =
    case status of
        Success a ->
            a

        _ ->
            defaultValue


init : E.Value -> ( Model, Cmd Msg )
init value =
    let
        initModel =
            { sampleTexts = Loading
            , collections = Loading
            , inputText = ""
            , visibility = Smart
            , selectedCollections = Set.empty
            , tempSelectedCollections = Set.empty
            }
    in
    ( case D.decodeValue Data.storeDecoder value of
        Ok store ->
            toModel initModel store

        -- TODO: Handle decoding error
        Err _ ->
            initModel
    , Cmd.batch
        [ Api.getSampleTexts GotSampleTexts
        , Api.getCollections GotCollections
        ]
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


fromModel : Model -> Data.Store
fromModel model =
    { version = 20231228
    , blacklistCollection = Set.toList model.selectedCollections
    , inputText = model.inputText
    }


toModel : Model -> Data.Store -> Model
toModel model store =
    { model
        | inputText = store.inputText
        , selectedCollections = Set.fromList store.blacklistCollection
    }


updateWithStore : Msg -> Model -> ( Model, Cmd Msg )
updateWithStore msg model =
    let
        ( newModel, cmds ) =
            update msg model
    in
    ( newModel
    , Cmd.batch [ Port.setStore (Data.storeEncoder (fromModel newModel)), cmds ]
    )


type Msg
    = GotSampleTexts (Result Http.Error (List Data.SampleText))
    | GotCollections (Result Http.Error (List Data.Collection))
    | SetInputText String
    | SampleTextSelected String
    | CollectionSelectionChanged String Bool
    | TriggerAnalyze
    | VisibilityChanged Visibility
    | ToggleModal ModalId
    | OpenBlacklistModal
    | CloseBlacklistModal Bool


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SampleTextSelected key ->
            let
                sampleText =
                    case model.sampleTexts of
                        Success texts ->
                            List.filter (\x -> x.id == key) texts

                        _ ->
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

                Err err ->
                    ( { model | sampleTexts = Failed (friendlyHttpError err) }, Cmd.none )

        TriggerAnalyze ->
            let
                _ =
                    Debug.log model.inputText ""
            in
            ( model, Cmd.none )

        VisibilityChanged visibility ->
            ( { model | visibility = visibility }, Cmd.none )

        ToggleModal modalId ->
            ( model, Port.toggleDialog (modalIdStr modalId) )

        GotCollections result ->
            case result of
                Ok data ->
                    ( { model | collections = Success data }, Cmd.none )

                Err err ->
                    ( { model | collections = Failed (friendlyHttpError err) }, Cmd.none )

        CollectionSelectionChanged collId isChecked ->
            let
                operation =
                    if isChecked then
                        Set.insert

                    else
                        Set.remove
            in
            ( { model | selectedCollections = operation collId model.selectedCollections }, Cmd.none )

        OpenBlacklistModal ->
            update
                (ToggleModal ModalBlacklist)
                { model | tempSelectedCollections = model.selectedCollections }

        CloseBlacklistModal isOk ->
            update
                (ToggleModal ModalBlacklist)
                { model
                    | tempSelectedCollections = Set.empty
                    , selectedCollections =
                        if isOk then
                            model.selectedCollections

                        else
                            model.tempSelectedCollections
                }


view : Model -> Html Msg
view model =
    div [ class "flex flex-col h-screen min-w-[375px]" ]
        [ viewHeader model
        , div [ class "grow bg-red-900 text-white" ] []
        , div [] [ viewMenu model, viewTextArea model ]
        , modal model ModalBlacklist "Blacklist" viewModalBlacklist (CloseBlacklistModal False)
        , modal model ModalAbout "About" viewModalAbout (ToggleModal ModalAbout)
        ]


modal : a -> ModalId -> String -> (a -> Html msg) -> msg -> Html msg
modal model modelId =
    Modal.modal model (modalIdStr modelId)


viewModalBlacklist : Model -> Html Msg
viewModalBlacklist model =
    div [ class "flex flex-col gap-4" ]
        [ div [ class "h-[55vh] md:h-[74vh] overflow-y-scroll" ]
            [ viewCollectionList model ]
        , div [ class "flex gap-2" ]
            [ button [ class "btn btn-primary", onClick (CloseBlacklistModal True) ]
                [ text "Ok" ]
            , button [ class "btn btn-error", onClick (CloseBlacklistModal False) ]
                [ text "Cancel" ]
            ]
        ]


viewCollectionList : Model -> Html Msg
viewCollectionList model =
    let
        mapSelected coll =
            coll
                |> List.map (\c -> ( c, Set.member c.id model.selectedCollections ))

        createCard ( collection, isChecked ) =
            viewCollectionItem collection isChecked

        container attr content =
            div (class "h-full w-full flex flex-col justify-center items-center" :: attr) content
    in
    case model.collections of
        Success coll ->
            div [ class "flex flex-col gap-1.5" ] (List.map createCard (mapSelected coll))

        Loading ->
            container [ class "gap-4" ]
                [ span [ class "loading loading-spinner text-primary loading-lg" ] []
                , span [ class "text-xl" ] [ text "Loading..." ]
                ]

        Failed err ->
            container [ class "gap-1" ]
                [ span [ class "text-3xl font-bold text-error" ] [ text "Error" ]
                , span [ class "text-xl text-error" ] [ text err.message ]
                , span [ class "text-xl text-error" ] [ text err.resolution ]
                ]


viewCollectionItem : Data.Collection -> Bool -> Html Msg
viewCollectionItem collection state =
    div [ class "flex items-center bg-gray-800 p-4 hover:bg-gray-900 rounded" ]
        [ div [ class "grow flex flex-col" ]
            [ h2 [ class "text-lg md:text-2xl" ] [ text collection.name ]
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
    div [ class "flex p-2 md:px-4 bg-blue-800 text-white items-center" ]
        [ div [ class "grow text-xl md:text-2xl font-bold" ] [ text "Hanzi Memo" ]
        , div [ class "flex items-center gap-2 md:gap-5" ]
            [ viewSelectTextPreset model
            , button [ onClick (ToggleModal ModalAbout), class "hidden md:block" ] [ text "About" ]

            -- TODO: change to three-vertical-dot icon
            , button [ onClick (ToggleModal ModalAbout), class "md:hidden" ] [ text "#" ]
            ]
        ]


viewSelectTextPreset : Model -> Html Msg
viewSelectTextPreset { sampleTexts } =
    select [ class "select md:select-md rounded-none", onInput SampleTextSelected ]
        (option [ hidden True ] [ text "Sample Text" ]
            :: List.map createOption (withDefault [] sampleTexts)
        )


viewTextArea : Model -> Html Msg
viewTextArea { inputText } =
    div [ class "flex relative h-40" ]
        [ textarea
            [ class "grow p-2 text-xl"
            , placeholder "Type something..."
            , value inputText
            , onInput SetInputText
            ]
            []
        , button [ class "btn btn-sm btn-primary absolute right-4 bottom-4", onClick TriggerAnalyze ] [ text "Analyze" ]
        ]


viewMenu : Model -> Html Msg
viewMenu model =
    div [ class "flex font-bold text-sm md:text-xl h-8 md:h-12 items-stretch bg-blue-800" ]
        [ viewBlacklistButton
        , div [ class "grow" ] []
        , viewVisibilityMenu model
        ]


viewBlacklistButton : Html Msg
viewBlacklistButton =
    button [ class "px-2 bg-white text-black", onClick OpenBlacklistModal ]
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
viewVisibilityMenu { visibility } =
    div [ class "flex h-full" ]
        (List.map (\x -> menuRadio x (visibility == Tuple.first x)) visibilityOptions)


menuRadio : ( Visibility, String ) -> Bool -> Html Msg
menuRadio ( isVisible, displayText ) isChecked =
    label
        [ class "flex items-center px-2 text-white font-bold hover:cursor-pointer"
        , classList [ ( "bg-white text-gray-900", isChecked ) ]
        ]
        [ input
            [ type_ "radio"
            , checked isChecked
            , onCheck (\_ -> VisibilityChanged isVisible)
            , hidden True
            ]
            []
        , text displayText
        ]


createOption : Data.SampleText -> Html Msg
createOption { id, title } =
    option [ value id ] [ text title ]
