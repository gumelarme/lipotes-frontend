module Api exposing (..)

import Env
import Http
import Json.Decode exposing (Decoder, field, list, map3, map4, string)


endpoint : String -> String
endpoint path =
    Env.hzm_api_url ++ path


dataDecoder : Decoder a -> Decoder (List a)
dataDecoder decoder =
    field "data" (list decoder)


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


getCollections : (Result Http.Error (List Collection) -> msg) -> Cmd msg
getCollections msg =
    Http.get
        { url = endpoint "/collections"
        , expect = Http.expectJson msg collectionDecoder
        }


type alias SampleText =
    { id : String
    , title : String
    , text : String
    }


getSampleTexts : (Result Http.Error (List SampleText) -> msg) -> Cmd msg
getSampleTexts msg =
    Http.get
        { url = endpoint "/texts"
        , expect = Http.expectJson msg textDecoder
        }


textDecoder : Decoder (List SampleText)
textDecoder =
    dataDecoder
        (map3 SampleText
            (field "id" string)
            (field "title" string)
            (field "text" string)
        )
