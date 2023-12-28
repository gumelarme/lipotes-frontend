module Api exposing (..)

import Data
import Env
import Http


endpoint : String -> String
endpoint path =
    Env.hzm_api_url ++ path


getCollections : (Result Http.Error (List Data.Collection) -> msg) -> Cmd msg
getCollections msg =
    Http.get
        { url = endpoint "/collections"
        , expect = Http.expectJson msg Data.collectionDecoder
        }


getSampleTexts : (Result Http.Error (List Data.SampleText) -> msg) -> Cmd msg
getSampleTexts msg =
    Http.get
        { url = endpoint "/texts"
        , expect = Http.expectJson msg Data.textDecoder
        }
