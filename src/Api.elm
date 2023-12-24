module Api exposing (..)

import Env

endpoint: String -> String
endpoint path =
    Env.hzm_api_url ++ path
