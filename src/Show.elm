module Show exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import List exposing (..)
import String exposing (..)


type alias Model =
    { param : String
    }


type Msg
    = EditParam String


fileBase : String
fileBase =
    "https://hkustconnect-my.sharepoint.com/personal/yhsha_connect_ust_hk/Documents/plots/"


years : List Int
years =
    range 2001 2021
        |> List.reverse


formatFileName : String -> Int -> String
formatFileName param year =
    fileBase
        ++ "intp_sea/2019/"
        ++ param
        ++ "/n_stats_10/"
        ++ param
        ++ "_"
        ++ String.fromInt year
        ++ "_10_tpu_mean.png"
    |> Debug.log fileBase



-- formatFileNameList : String -> List Int -> List String
-- formatFileNameList param years =
--     List.map ()


toLi : String -> Html msg
toLi str =
    li []
        [ img [ src str ] []
        ]


toTd : String -> Html msg
toTd str =
    td []
        [ img [ src str ] []
        ]


toTr : List String -> Html msg
toTr strs =
    tr [] (List.map toTd strs)


toHtmlTable : List String -> Html msg
toHtmlTable strs =
    table [] [ toTr strs ]


toHtmlList : List String -> Html msg
toHtmlList strs =
    ul [] (List.map toLi strs)


update : Msg -> Model -> Model
update msg model =
    case msg of
        EditParam param ->
            { model | param = param }


init : Model
init =
    Model "NO2"



-- view : Model -> Html Msg
-- view model =
--     div []
--         [ img [ src "https://hkustconnect-my.sharepoint.com/personal/yhsha_connect_ust_hk/Documents/plots/intp_sea/2019/NO2/n_stats_10/NO2_2001_10_tpu_mean.png" ] []
--         ]
--
--
-- type Msg
--     = Increment
--     | Decrement
--f
--
-- update msg model =
--     model
--
--
-- initialModel : Model
-- initialModel =
--     "A"
--
--
-- main : Program () Model Msg
-- main =
--     Browser.sandbox
--         { init = initialModel
--         , view = view
--         , update = update
-- {-         } -}


params : List String
params =
    [ "NO2"
    , "O3"
    , "O3_max_8hr_avg"
    , "SO2"
    ]


listOptional : String -> Html msg
listOptional opt =
    option [ value opt ] [ text opt ]


paramSelect : Html Msg
paramSelect =
    select
        [ onInput EditParam ]
        (List.map listOptional params)


view : Model -> Html Msg
view model =
    div []
        [ paramSelect
        , List.map (\year -> formatFileName model.param year) years
            |> toHtmlTable
        ]


main : Program () Model Msg
main =
    Browser.sandbox
        { init = init
        , view = view
        , update = update
        }
