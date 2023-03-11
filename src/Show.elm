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



-- constants


params : List String
params =
    [ "NO2"
    , "O3"
    , "O3_max_8hr_avg"
    , "SO2"
    ]


res : List String
res =
    [ "raw", "TPU", "DCD" ]


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
    [ "sf", "intp_simple", "intp_sea", "r_nrmse" ]


subModelNames : List String
subModelNames =
    [ "r_70_nrmse_20"
    , "r_70_nrmse_30"
    , "r_70_nrmse_40"
    , "r_70_nrmse_50"
    , "r_80_nrmse_20"
    , "r_80_nrmse_30"
    , "r_80_nrmse_40"
    , "r_80_nrmse_50"
    , "r_90_nrmse_20"
    , "r_90_nrmse_30"
    , "r_90_nrmse_40"
    , "r_90_nrmse_50"
    ]


type alias BCType =
    { id : String
    , name : String
    , subname : String
    , base : String
    , n_stats : String
    }


type alias Model =
    { param : String
    , res : String
    , bcTypes : List BCType
    , newName : String
    , newSubname : String
    , newBase : String
    , newNStats : String
    , nRecords : Int
    , switched : Bool
    }


type Msg
    = EditParam String
    | EditRes String
    | AddSelectNameMsg String
    | AddSelectSubnameMsg String
    | AddSelectBaseMsg String
    | AddSelectNStatsMsg String
    | AddButtonMsg
    | DeleteButtonMsg String


formatFileName : String -> String -> Int -> BCType -> String
formatFileName resolution param year bctype =
    let
        modelSubnameStr =
            if bctype.name == "r_nrmse" then
                bctype.subname ++ "/"

            else
                ""

        nStatsStr =
            modelSubnameStr ++ "n_stats_" ++ bctype.n_stats

        resStr =
            case resolution of
                "raw" ->
                    "spatial"

                "TPU" ->
                    "tpu_mean"

                "DCD" ->
                    "dcd_mean"

                _ ->
                    "spatial"

        resSuffix =
            case resolution of
                "raw" ->
                    ""

                "TPU" ->
                    "tpu_mean"

                "DCD" ->
                    "dcd_mean"

                _ ->
                    ""

        fileNameRoot =
            param
                ++ "_"
                ++ String.fromInt year
                ++ "_"
                ++ pad 2 '0' bctype.n_stats
                ++ resSuffix
                ++ ".png"
    in
    String.join "/"
        [ fileBase
        , resStr
        , bctype.name
        , bctype.base
        , param
        , nStatsStr
        , fileNameRoot
        ]


toTd : String -> String -> Int -> BCType -> Html msg
toTd resolution param year bctype =
    let
        str =
            formatFileName resolution param year bctype
    in
    td [] [ img [ src str ] [] ]


toTr : String -> String -> Int -> List BCType -> Html msg
toTr resolution param year bctypeList =
    tr []
        (td [] [ String.fromInt year |> text ]
            :: List.map
                (\bctype -> toTd resolution param year bctype)
                bctypeList
        )


formatBCName : BCType -> String
formatBCName bctype =
    let
        subnameStr =
            if String.isEmpty bctype.subname then
                ""

            else
                "\n" ++ bctype.subname
    in
    bctype.name
        ++ subnameStr
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


toHtmlTable : String -> String -> List Int -> List BCType -> Html Msg
toHtmlTable resolution param yearList bctypeList =
    table [ class "center" ]
        (toThead bctypeList
            :: List.map 
            (\year -> toTr resolution param year bctypeList) 
            yearList
        )


toBCTypesTr : BCType -> Table.Row Msg
toBCTypesTr bctype =
    Table.tr [ Table.rowAttr (id bctype.id) ]
        [ Table.td [] [ text bctype.name ]
        , Table.td [] [ text bctype.subname ]
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
        { options = [ Table.small, Table.hover, Table.attr (class "table-fixed") ]
        , thead =
            Table.simpleThead
                [ Table.th [] [ text "Name" ]
                , Table.th [] [ text "Subname" ]
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

        EditRes resolution ->
            ( { model | res = resolution }, Cmd.none )

        AddSelectNameMsg name ->
            let
                nStats =
                    if left 4 name == "intp" then
                        "3"

                    else
                        "1"

                subname =
                    if name == "r_nrmse" then
                        "r_70_nrmse_20"

                    else
                        ""
            in
            Debug.log name
                ( { model
                    | newName = name
                    , newNStats = nStats
                    , newSubname = subname
                  }
                , Cmd.none
                )

        AddSelectSubnameMsg subname ->
            ( { model | newSubname = subname }, Cmd.none )

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
                        model.newSubname
                        model.newBase
                        model.newNStats

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
        "raw"
        [ BCType "bc1" "sf" "" "2019" "7"
        , BCType "bc2" "intp_simple" "" "2019" "7"
        , BCType "bc3" "intp_sea" "" "2019" "7"
        ]
        "sf"
        ""
        "2019"
        "1"
        3
        False


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


resSelect : Model -> Html Msg
resSelect model =
    Select.select
        [ Select.id "resSelect"
        , Select.onChange EditRes
        ]
        (List.map listOptions res)


inputForm : Model -> Html Msg
inputForm model =
    let
        nStatsRange =
            if left 4 model.newName == "intp" then
                List.map
                    (String.fromInt
                        >> (\opt -> listOptionsSelected opt model.newNStats)
                    )
                    (range 3 11)

            else
                List.map
                    (String.fromInt
                        >> (\opt -> listOptionsSelected opt model.newNStats)
                    )
                    (range 1 14)

        -- show subnames only when
        modelSubnameList =
            if model.newName == "r_nrmse" then
                subModelNames

            else
                [ "-------------" ]
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
            [ Form.label [ for "subnameInput" ] [ text "Subname: " ]
            , Select.select
                [ Select.id "subnameInput"
                , Select.attrs [ class "ml-2 mr-3 my-2" ]
                , Select.onChange AddSelectSubnameMsg
                ]
                (List.map listOptions modelSubnameList)
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
        , div [ class "container w-25" ]
            [ text "Resolution: "
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
            [ img [ src (fileBase ++ "/conc_cb.png"), class "img-center" ] [] ]
        , br [] []
        , div [ class "center" ]
            [ toHtmlTable model.res model.param years model.bcTypes ]
        ]


main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> ( init, Cmd.none )
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }
