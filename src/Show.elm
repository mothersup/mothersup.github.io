module Show exposing (main)

import Array exposing (..)
import Bootstrap.Button as Button
import Bootstrap.Form as Form
import Bootstrap.Form.Select as Select
import Bootstrap.Table as Table
import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import List exposing (..)
import List.Extra exposing (..)
import String exposing (..)


type alias BCType =
    { id : String
    , name : String
    , base : String
    , n_stats : String
    , subtype : String
    }


type alias Model =
    { param : String
    , bcTypes : List BCType
    , newName : String
    , newBase : String
    , newNStats : String
    , nRecords : Int
    , switched : Bool
    }


type Msg
    = EditParam String
    | AddSelectNameMsg String
    | AddSelectBaseMsg String
    | AddSelectNStatsMsg String
    | AddButtonMsg
    | DeleteButtonMsg String


fileBase : String
fileBase =
    "https://hkustconnect-my.sharepoint.com/personal/yhsha_connect_ust_hk/Documents/plots"


years : List Int
years =
    range 2001 2021
        |> List.reverse


baseYears : List Int
baseYears =
    range 2019 2021


modelNames : List String
modelNames =
    [ "sf", "intp_simple", "intp_sea" ]


formatFileName : String -> Int -> BCType -> String
formatFileName param year bctype =
    let
        nStatsStr =
            "n_stats_" ++ bctype.n_stats

        fileNameRoot =
            param
                ++ "_"
                ++ String.fromInt year
                ++ "_"
                ++ pad 2 '0' bctype.n_stats
                ++ "_tpu_mean.png"
    in
    String.join "/"
        [ fileBase
        , bctype.name
        , bctype.base
        , param
        , nStatsStr
        , fileNameRoot
        ]


toLi : String -> Html msg
toLi str =
    li []
        [ img [ src str ] []
        ]


toTd : String -> Int -> BCType -> Html msg
toTd param year bctype =
    let
        str =
            formatFileName param year bctype
    in
    td [] [ img [ src str ] [] ]


toTr : String -> Int -> List BCType -> Html msg
toTr param year bctypeList =
    tr []
        (td [] [ String.fromInt year |> text ]
            :: List.map (\bctype -> toTd param year bctype) bctypeList
        )


formatBCName : BCType -> String
formatBCName bctype =
    bctype.name
        ++ "\nN stations: "
        ++ bctype.n_stats
        ++ "\nBase year: "
        ++ bctype.base


toTh : String -> Html Msg
toTh str =
    th [] [ text str ]


toThead : List BCType -> Html Msg
toThead bctypeList =
    thead []
        (th [] [ text "Year" ]
            :: List.map (formatBCName >> toTh) bctypeList
        )


toHtmlTable : String -> List Int -> List BCType -> Html Msg
toHtmlTable param yearList bctypeList =
    table [ class "center" ]
        (toThead bctypeList
            :: List.map (\year -> toTr param year bctypeList) yearList
        )


toBCTypesTr : BCType -> Table.Row Msg
toBCTypesTr bctype =
    Table.tr [ Table.rowAttr (id bctype.id) ]
        [ Table.td [] [ text bctype.name ]
        , Table.td [] [ text bctype.base ]
        , Table.td [] [ text bctype.n_stats ]
        , Table.td []
            [ Button.button
                [ Button.danger
                , Button.onClick (DeleteButtonMsg bctype.id)
                ]
                [ text "delete" ]
            ]
        ]


showBCTypesTable : List BCType -> Html Msg
showBCTypesTable bctypeList =
    Table.table
        { options = [ Table.small, Table.hover, Table.attr (class "table-fixed")]
        , thead =
            Table.simpleThead
                [ Table.th [] [ text "Name" ]
                , Table.th [] [ text "N stations" ]
                , Table.th [] [ text "Base year" ]
                , Table.th [] [ text "Delete row" ]
                ]
        , tbody =
            Table.tbody 
            [] 
            (List.map toBCTypesTr bctypeList)
        }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        EditParam param ->
            ( { model | param = param }, Cmd.none )

        AddSelectNameMsg name ->
            let
                nStats =
                    if left 4 name == "intp" then
                        "3"

                    else
                        "1"
            in
            Debug.log name
                ( { model
                    | newName = name
                    , newNStats = nStats
                  }
                , Cmd.none
                )

        AddSelectBaseMsg base ->
            ( { model | newBase = base }, Cmd.none )

        AddSelectNStatsMsg nStats ->
            ( { model | newNStats = nStats }, Cmd.none )

        AddButtonMsg ->
            let
                incRecord =
                    model.nRecords + 1

                newId =
                    "bc" ++ String.fromInt incRecord

                newBCType =
                    BCType
                        newId
                        model.newName
                        model.newBase
                        model.newNStats
                        ""

                newModel =
                    { model
                        | bcTypes = model.bcTypes ++ [ newBCType ]
                        , nRecords = incRecord
                    }
            in
            ( newModel, Cmd.none )

        DeleteButtonMsg id ->
            let
                index =
                    findIndex (\bcType -> bcType.id == id) model.bcTypes

                newBCType =
                    case index of
                        Nothing ->
                            model.bcTypes

                        Just justIndex ->
                            removeAt justIndex model.bcTypes
            in
            ( { model | bcTypes = newBCType }, Cmd.none )


init : Model
init =
    Model
        "NO2"
        [ BCType "bc1" "sf" "2019" "7" ""
        , BCType "bc2" "intp_simple" "2019" "7" ""
        , BCType "bc3" "intp_sea" "2019" "7" ""
        ]
        "sf"
        "2019"
        "1"
        3
        False


params : List String
params =
    [ "NO2"
    , "O3"
    , "O3_max_8hr_avg"
    , "SO2"
    ]


listOptions : String -> Select.Item Msg
listOptions opt =
    Select.item [ value opt ] [ text opt ]


listOptionsSelected : String -> String -> Select.Item Msg
listOptionsSelected opt default =
    Select.item
        [ attribute "value" opt
        , selected (opt == default)
        ]
        [ text opt ]


paramSelect : Model -> Html Msg
paramSelect model =
    Select.select
        [ Select.id "paramSelect"
        , Select.onChange EditParam
        ]
        (List.map listOptions params)


inputForm : Model -> Html Msg
inputForm model =
    let
        nStatsRange =
            if model.newName == "sf" then
                List.map
                    (String.fromInt
                        >> (\opt -> listOptionsSelected opt model.newNStats)
                    )
                    (range 1 14)

            else
                List.map
                    (String.fromInt
                        >> (\opt -> listOptionsSelected opt model.newNStats)
                    )
                    (range 3 11)
    in
    Form.formInline [ class "justify-content-center" ]
        [ Form.group []
            [ Form.label [ for "nameInput" ] [ text "Name: " ]
            , Select.select
                [ Select.id "nameInput"
                , Select.attrs [ class "ml-2 mr-3 my-2" ]
                , Select.onChange AddSelectNameMsg
                ]
                (List.map listOptions modelNames)
            ]
        , Form.group []
            [ Form.label [ for "baseInput" ] [ text "Base year: " ]
            , Select.select
                [ Select.id "baseInput"
                , Select.attrs [ class "ml-2 mr-3 my-2" ]
                , Select.onChange AddSelectBaseMsg
                ]
                (List.map (String.fromInt >> listOptions) baseYears)
            ]
        , Form.group []
            [ Form.label [ for "nStatsInput" ] [ text "N stations: " ]
            , Select.select
                [ Select.id "nStatsInput"
                , Select.attrs [ class "ml-2 mr-3 my-2" ]
                , Select.onChange AddSelectNStatsMsg
                ]
                nStatsRange
            ]
        , Button.button
            [ Button.primary, Button.onClick AddButtonMsg ]
            [ text "Add model" ]
        ]


view : Model -> Html Msg
view model =
    div []
        [ div [ class "container w-25" ]
            [ text "Parameter: "
            , paramSelect model
            ]
        , br [] []
        , div
            [ class "col-auto table-responsive container"
            ]
            [ showBCTypesTable model.bcTypes ]
        , div []
            [ inputForm model ]
        , br [] []
        , div [] 
            [ img [ src ( fileBase ++ "/conc_cb.png" ), class "img-center"] []]
        , br [] []
        , div [ class "center" ]
            [ toHtmlTable model.param years model.bcTypes ]
        ]


main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> ( init, Cmd.none )
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }
