module Main exposing (..)

import Api
import Browser
import Dict exposing (Dict)
import Html exposing (Html, button, div, h2, input, label, option, p, select, span, text, textarea)
import Html.Attributes exposing (checked, class, classList, hidden, id, placeholder, type_, value)
import Html.Events exposing (onCheck, onClick, onInput)
import Http
import Json.Decode as D
import Json.Encode as E
import List
import Modal
import Port


type alias Store =
    { version : Int
    , blacklistCollection : List String
    , inputText : String
    }


storeEncoder : Store -> E.Value
storeEncoder store =
    E.object
        [ ( "version", E.int store.version )
        , ( "blacklistCollection", E.list E.string store.blacklistCollection )
        , ( "inputText", E.string store.inputText )
        ]


storeDecoder : D.Decoder Store
storeDecoder =
    D.map3 Store
        (D.field "version" D.int)
        (D.field "blacklistCollection" (D.list D.string))
        (D.field "inputText" D.string)


main : Program E.Value Model Msg
main =
    Browser.element { init = init, update = updateWithStore, view = view, subscriptions = subscriptions }


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


type alias Model =
    { inputText : String
    , visibility : Visibility
    , sampleTexts : Status (List Api.SampleText)
    , collections : Status (List ( Api.Collection, Bool ))
    , modalPrevSelectedCollection : Dict String Bool
    }


type Status a
    = Loading
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
            , modalPrevSelectedCollection = Dict.empty
            }
    in
    ( case D.decodeValue storeDecoder value of
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


fromModel : Model -> Store
fromModel model =
    { version = 20231228
    , blacklistCollection =
        withDefault [] model.collections
            |> List.filter (\( _, state ) -> state)
            |> List.unzip
            |> Tuple.first
            |> List.map (\x -> x.id)
    , inputText = model.inputText
    }


toModel : Model -> Store -> Model
toModel model store =
    { model
        | inputText = store.inputText
        , modalPrevSelectedCollection =
            store.blacklistCollection
                |> List.map (\x -> ( x, True ))
                |> Dict.fromList
    }


updateWithStore : Msg -> Model -> ( Model, Cmd Msg )
updateWithStore msg model =
    let
        ( newModel, cmds ) =
            update msg model
    in
    ( newModel
    , Cmd.batch [ Port.setStore (storeEncoder (fromModel model)), cmds ]
    )


type Msg
    = GotSampleTexts (Result Http.Error (List Api.SampleText))
    | GotCollections (Result Http.Error (List Api.Collection))
    | SetInputText String
    | SampleTextSelected String
    | CollectionSelectionChanged String Bool
    | TriggerAnalyze
    | VisibilityChanged Visibility
    | ToggleModal ModalId (Cmd Msg)
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

        ToggleModal modalId doAfter ->
            ( model
            , Cmd.batch
                [ Port.toggleDialog (modalIdStr modalId)
                , doAfter
                ]
            )

        -- ( model, Port.toggleDialog (modalIdStr modalId) )
        GotCollections result ->
            case result of
                Ok data ->
                    ( { model | collections = Success (List.map (\x -> ( x, False )) data) }, Cmd.none )

                Err _ ->
                    ( { model | collections = Loading }, Cmd.none )

        CollectionSelectionChanged collId isChecked ->
            let
                collections =
                    withDefault [] model.collections

                toggleStatus pair =
                    if (Tuple.first pair).id == collId then
                        ( Tuple.first pair, isChecked )

                    else
                        pair
            in
            ( { model | collections = Success (List.map toggleStatus collections) }
            , Cmd.none
            )

        OpenBlacklistModal ->
            let
                collectionStates =
                    List.map (Tuple.mapFirst (\x -> x.id)) (withDefault [] model.collections)
            in
            update (ToggleModal ModalBlacklist Cmd.none) { model | modalPrevSelectedCollection = Dict.fromList collectionStates }

        CloseBlacklistModal isOk ->
            let
                getPrevState { id } state =
                    Maybe.withDefault state (Dict.get id model.modalPrevSelectedCollection)

                previousStates =
                    List.map (\( coll, state ) -> ( coll, getPrevState coll state )) (withDefault [] model.collections)
            in
            if isOk then
                update
                    (ToggleModal ModalBlacklist Cmd.none)
                    { model | modalPrevSelectedCollection = Dict.empty }

            else
                update
                    (ToggleModal ModalBlacklist Cmd.none)
                    { model
                        | modalPrevSelectedCollection = Dict.empty
                        , collections = Success previousStates
                    }


view : Model -> Html Msg
view model =
    div []
        [ viewHeader model
        , div [] [ viewMenu model, viewTextArea model ]
        , modal model ModalBlacklist "Blacklist" viewModalBlacklist (CloseBlacklistModal False)
        , modal model ModalAbout "About" viewModalAbout (ToggleModal ModalAbout Cmd.none)
        ]


modal : a -> ModalId -> String -> (a -> Html msg) -> msg -> Html msg
modal model modelId =
    Modal.modal model (modalIdStr modelId)


viewModalBlacklist : Model -> Html Msg
viewModalBlacklist model =
    div [ class "flex flex-col gap-4" ]
        [ div [ class "h-[74vh] overflow-y-scroll" ]
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
        collections =
            withDefault [] model.collections

        createCard ( collection, isChecked ) =
            viewCollectionItem collection isChecked

        collectionCards =
            List.map createCard collections
    in
    case model.collections of
        Success _ ->
            div [ class "flex flex-col gap-1.5" ] collectionCards

        Loading ->
            div [ class "h-full w-full flex flex-col gap-4 justify-center items-center" ]
                [ span [ class "loading loading-spinner text-primary loading-lg" ] []
                , span [ class "text-xl" ] [ text "Loading..." ]
                ]


viewCollectionItem : Api.Collection -> Bool -> Html Msg
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
            , button [ onClick (ToggleModal ModalAbout Cmd.none) ] [ text "About" ]
            ]
        ]


viewSelectTextPreset : Model -> Html Msg
viewSelectTextPreset { sampleTexts } =
    select [ class "select select-md rounded-none", onInput SampleTextSelected ]
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
    div [ class "flex font-bold text-xl h-12 items-stretch bg-blue-800" ]
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
        , classList [ ( "bg-white text-black", isChecked ) ]
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


createOption : Api.SampleText -> Html Msg
createOption { id, title } =
    option [ value id ] [ text title ]
