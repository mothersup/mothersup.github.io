module Show exposing (main)

import Array exposing (..)
import Bootstrap.Button as Button
import Bootstrap.Form as Form
import Bootstrap.Form.Select as Select
import Bootstrap.Table as Table
import Bootstrap.Utilities.Spacing as Spacing
import Browser
import Dict exposing (..)
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
    , "O3 (max 8hr avg)"
    , "SO2"
    , "PM2.5"
    ]


resList : List String
resList =
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


defaultModels : Dict String BCType
defaultModels =
    Dict.fromList
        [ ( "NO2", BCType "0" "r_nrmse" "r_90_nrmse_50" "2019" "3" )
        , ( "O3 (max 8hr avg)", BCType "0" "r_nrmse" "r_70_nrmse_40" "2019" "5" )
        , ( "SO2", BCType "0" "r_nrmse" "r_70_nrmse_40" "2021" "3" )
        , ( "PM2.5", BCType "0" "r_nrmse" "r_70_nrmse_50" "2021" "4" )
        ]


defaultBCType : BCType
defaultBCType =
    BCType "0" "r_nrmse" "r_90_nrmse_50" "2019" "3"


type Msg
    = EditParam String
    | EditRes String
    | AddSelectNameMsg String
    | AddSelectSubnameMsg String
    | AddSelectBaseMsg String
    | AddSelectNStatsMsg String
    | AddButtonMsg
    | AddDefaultButtonMsg
    | DeleteButtonMsg String


{-| Create a string for the file name of the plot specified 
by the resolution, parameter, year and BCType
-}
formatFileName : String -> String -> Int -> BCType -> String
formatFileName resolution param year bctype =
    let
        modelSubnameStr =
            if bctype.name == "r_nrmse" then
                "/" ++ bctype.subname

            else
                ""

        nStatsStr =
            "n_stats_" ++ bctype.n_stats ++ modelSubnameStr

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
                    "_tpu_mean"

                "DCD" ->
                    "_dcd_mean"

                _ ->
                    ""

        varSaveName =
            case param of
                "O3 (max 8hr avg)" ->
                    "O3_max_8hr_avg"

                "PM2.5" ->
                    "PM25"

                _ ->
                    param

        fileNameRoot =
            varSaveName
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
        , varSaveName
        , nStatsStr
        , fileNameRoot
        ]


{-| Display image as a table entry
-}
toTd : String -> String -> Int -> BCType -> Html msg
toTd resolution param year bctype =
    let
        str =
            formatFileName resolution param year bctype
    in
    td [] [ img [ src str ] [] ]


{-| Display images in a row in  a table
-}
toTr : String -> String -> Int -> List BCType -> Html msg
toTr resolution param year bctypeList =
    tr []
        (td [] [ String.fromInt year |> text ]
            :: List.map
                (\bctype -> toTd resolution param year bctype)
                bctypeList
        )


toPlotTh : BCType -> String -> Html Msg
toPlotTh bctype field =
    let
        value =
            case field of
                "name" ->
                    bctype.name

                "subname" ->
                    bctype.subname

                "n_stats" ->
                    bctype.n_stats

                "base" ->
                    bctype.base

                _ ->
                    bctype.name
    in
    th [ class "th_center" ] [ text value ]


toThead : List BCType -> Html Msg
toThead bctypeList =
    thead []
        [ tr []
            (th [ class "th_center" ] [ text "Name" ]
                :: List.map (\bctype -> toPlotTh bctype "name") bctypeList
            )
        , tr []
            (th [ class "th_center" ] [ text "Subname" ]
                :: List.map (\bctype -> toPlotTh bctype "subname") bctypeList
            )
        , tr []
            (th [ class "th_center" ] [ text "N stations" ]
                :: List.map (\bctype -> toPlotTh bctype "n_stats") bctypeList
            )
        , tr []
            (th [ class "th_center" ] [ text "Base year" ]
                :: List.map (\bctype -> toPlotTh bctype "base") bctypeList
            )
        ]


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
                -- change range of number of station
                -- intp_sea or intp_simple: 3 to 11
                -- sf or r_nrmse: 1 to 14
                nStats =
                    if left 4 name == "intp" then
                        "3"

                    else
                        "1"

                -- set default subname for r_nrmse
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

        AddDefaultButtonMsg ->
            let
                incRecord =
                    model.nRecords + 1

                newId =
                    "bc" ++ String.fromInt incRecord

                newBCTypeTmplt =
                    Dict.get model.param defaultModels
                        |> Maybe.withDefault defaultBCType

                newBCType =
                    { newBCTypeTmplt | id = newId }

                newModel =
                    { model
                        | bcTypes = model.bcTypes ++ [ newBCType ]
                        , nRecords = incRecord
                    }
            in
            ( newModel, Cmd.none )

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
        [ BCType "bc1" "r_nrmse" "r_90_nrmse_50" "2019" "3"
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



-- paramSelect : Model -> Html Msg
-- paramSelect model =


paramSelect : Html Msg
paramSelect =
    Select.select
        [ Select.id "paramSelect"
        , Select.attrs [ Spacing.ml2, Spacing.mr4 ]
        , Select.onChange EditParam
        ]
        (List.map listOptions params)



-- resSelect : Model -> Html Msg
-- resSelect model =


resSelect : Html Msg
resSelect =
    Select.select
        [ Select.id "resSelect"
        , Select.attrs [ Spacing.ml2, Spacing.mr4 ]
        , Select.onChange EditRes
        ]
        (List.map listOptions resList)


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
                , Select.attrs [ Spacing.mx2 ]
                , Select.onChange AddSelectNameMsg
                ]
                (List.map listOptions modelNames)
            ]
        , Form.group []
            [ Form.label [ for "subnameInput" ] [ text "Subname: " ]
            , Select.select
                [ Select.id "subnameInput"
                , Select.attrs [ Spacing.mx2 ]
                , Select.onChange AddSelectSubnameMsg
                ]
                (List.map listOptions modelSubnameList)
            ]
        , Form.group []
            [ Form.label [ for "baseInput" ] [ text "Base year: " ]
            , Select.select
                [ Select.id "baseInput"
                , Select.attrs [ Spacing.mx2 ]
                , Select.onChange AddSelectBaseMsg
                ]
                (List.map (String.fromInt >> listOptions) baseYears)
            ]
        , Form.group []
            [ Form.label [ for "nStatsInput" ] [ text "N stations: " ]
            , Select.select
                [ Select.id "nStatsInput"
                , Select.attrs [ Spacing.mx2 ]
                , Select.onChange AddSelectNStatsMsg
                ]
                nStatsRange
            ]
        , Button.button
            [ Button.primary
            , Button.attrs [ Spacing.mx2 ]
            , Button.onClick AddButtonMsg
            ]
            [ text "Add model" ]
        , Button.button
            [ Button.info
            , Button.attrs [ Spacing.mx2 ]
            , Button.onClick AddDefaultButtonMsg
            ]
            [ text "Add default" ]
        ]


view : Model -> Html Msg
view model =
    div []
        [ Form.formInline [ class "justify-content-center" ]
            [ Form.group []
                [ Form.label [ for "paramSelect" ] [ text "Parameter: " ]
                , paramSelect
                ]
            , Form.group []
                [ Form.label [ for "resSelect" ] [ text "Resolution: " ]
                , resSelect
                ]
            ]
        , br [] []
        , div
            [ class "plots_table"
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
