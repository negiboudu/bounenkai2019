port module Main exposing (..)

import Array
import Browser
import Browser.Events
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Encode
import Random
import Time


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type Rollstatus
    = Stop
    | Rolling


type alias Model =
    { rollstatus : Rollstatus
    , tempSelection : Int
    , waitTime : Int
    , fullGroup : List Int
    , unselectedGroup : List Int
    , selectedGroup : List Int
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model Stop 0 100 createGroup createGroup createGroup, Cmd.none )


createGroup : List Int
createGroup =
    Array.initialize 100 identity |> Array.toList


type Msg
    = Rollend
    | RandomGenerate Int
    | Rollstart Json.Encode.Value
    | ComeonAnimationFrame Time.Posix


port rollend : Int -> Cmd msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Rollstart _ ->
            ( { model | rollstatus = Rolling, waitTime = model.waitTime + 100 }, Cmd.none )

        Rollend ->
            ( { model | rollstatus = Stop, selectedGroup = model.selectedGroup ++ [ model.tempSelection ], unselectedGroup = Array.fromList model.unselectedGroup |> Array.filter (\val -> val /= model.tempSelection) |> Array.toList }, Cmd.none )

        RandomGenerate num ->
            ( { model | tempSelection = num }
            , Cmd.none
            )

        ComeonAnimationFrame _ ->
            ( model
            , if model.rollstatus == Rolling then
                Random.generate RandomGenerate <| Random.int 0 <| List.length model.unselectedGroup

              else
                Cmd.none
            )


port rollstart : (Json.Encode.Value -> msg) -> Sub msg


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch [ rollstart Rollstart, Browser.Events.onAnimationFrame ComeonAnimationFrame ]


view : Model -> Html Msg
view model =
    div []
        [ button
            [ onClick (Rollstart <| Json.Encode.int 0) ]
            [ text "まわす" ]
        , button
            [ onClick Rollend ]
            [ text "とめる" ]
        , div []
            [ text <| "rollstatus:" ++ Debug.toString model ]
        , table [ style "border-collapse" "collapse" ]
            [ tr []
                (model.fullGroup
                    |> List.map String.fromInt
                    |> List.map text
                    |> List.map (\html -> td [ style "border" "1px solid" ] [ html ])
                )
            ]
        ]
