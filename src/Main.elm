port module Main exposing (..)

import Array exposing (..)
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
    , unselectedGroup : Array Int
    , selectedGroup : Array Int
    , animationCount : Int
    , animationInterval : Int
    , animationIntervalLimit : Int
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model Stop 0 100 (Array.toList createGroup) createGroup createGroup 0 0 0, Cmd.none )


createGroup : Array Int
createGroup =
    Array.initialize 100 identity


type Msg
    = Rollend
    | RandomGenerate Int
    | Rollstart Json.Encode.Value
    | ComeonAnimationFrame Time.Posix


port rollend : Int -> Cmd msg


getSelection : Model -> Int
getSelection model =
    case Array.get model.tempSelection model.unselectedGroup of
        Just selection ->
            selection

        Nothing ->
            -1


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Rollstart _ ->
            ( { model | rollstatus = Rolling, waitTime = model.waitTime + 100 }, Cmd.none )

        Rollend ->
            ( { model
                | rollstatus = Stop
                , selectedGroup = Array.push (getSelection model) model.selectedGroup
                , unselectedGroup = Array.filter (\val -> val /= model.tempSelection) model.unselectedGroup
              }
            , Cmd.none
            )

        RandomGenerate num ->
            ( { model | tempSelection = num }
            , Cmd.none
            )

        ComeonAnimationFrame _ ->
            if model.rollstatus == Rolling then
                ( { model
                    | animationCount = model.animationCount + 1
                  }
                , if model.animationInterval < model.animationCount then
                    Random.generate RandomGenerate <| Random.int 0 <| Array.length model.unselectedGroup

                  else
                    Cmd.none
                )

            else
                ( model, Cmd.none )


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
