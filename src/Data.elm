module Data exposing (..)

import Json.Decode as D
import Json.Encode as E


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


dataDecoder : D.Decoder a -> D.Decoder (List a)
dataDecoder decoder =
    D.field "data" (D.list decoder)


type alias Lexeme =
    { id : String
    , zh_sc : String
    , zh_tc : String
    , pinyin : String
    }


lexemeDecoder : D.Decoder Lexeme
lexemeDecoder =
    D.map4 Lexeme
        (D.field "id" D.string)
        (D.field "zh_sc" D.string)
        (D.field "zh_tc" D.string)
        (D.field "pinyin" D.string)


type alias Collection =
    { id : String
    , name : String
    , preview : List Lexeme
    }


collectionDecoder : D.Decoder (List Collection)
collectionDecoder =
    dataDecoder
        (D.map3 Collection
            (D.field "id" D.string)
            (D.field "name" D.string)
            (D.field "preview" (D.list lexemeDecoder))
        )


type alias SampleText =
    { id : String
    , title : String
    , text : String
    }


textDecoder : D.Decoder (List SampleText)
textDecoder =
    dataDecoder
        (D.map3 SampleText
            (D.field "id" D.string)
            (D.field "title" D.string)
            (D.field "text" D.string)
        )
