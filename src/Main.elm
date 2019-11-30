port module Main exposing (..)

import Array exposing (..)
import Browser
import Browser.Events
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Encode
import Mixin exposing (Mixin)
import Neat
import Neat.Layout as Layout
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
    , fullGroup : List Int
    , unselectedGroup : Array Int
    , selectedGroup : Array Int
    , animationCount : Int
    , animationInterval : Int
    , animationIntervalLimit : Int
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { rollstatus = Stop
      , tempSelection = -1
      , fullGroup = Array.toList createGroup
      , unselectedGroup = createGroup
      , selectedGroup = Array.empty
      , animationCount = 0
      , animationInterval = 0
      , animationIntervalLimit = 20
      }
    , Cmd.none
    )


createGroup : Array Int
createGroup =
    Array.initialize 10 identity


type Msg
    = RandomGenerate Int
    | Rollstart Json.Encode.Value
    | ComeonAnimationFrame Time.Posix


port rollend : Int -> Cmd msg


getSelection : Array Int -> Int -> Int
getSelection arr index =
    case Array.get index arr of
        Just selection ->
            selection

        Nothing ->
            -1


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Rollstart _ ->
            ( { model | rollstatus = Rolling }, Cmd.none )

        RandomGenerate num ->
            ( { model
                | tempSelection = getSelection model.unselectedGroup num
                , animationCount = 0
                , animationInterval = model.animationInterval + 1
              }
            , Cmd.none
            )

        ComeonAnimationFrame _ ->
            if model.rollstatus == Rolling then
                ( if model.animationInterval > model.animationIntervalLimit then
                    { model
                        | rollstatus = Stop
                        , selectedGroup = Array.push model.tempSelection model.selectedGroup
                        , unselectedGroup = Array.filter (\val -> val /= model.tempSelection) model.unselectedGroup
                        , animationCount = 0
                        , animationInterval = 0
                    }

                  else
                    { model
                        | animationCount = model.animationCount + 1
                    }
                , if model.animationInterval < model.animationCount then
                    Array.length model.unselectedGroup
                        |> (\n -> n - 1)
                        |> Random.int 0
                        |> Random.generate RandomGenerate

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
    Layout.column
        [ Neat.lift Html.button
            [ onClick (Rollstart <| Json.Encode.int 0)
                |> Mixin.fromAttribute
            ]
            []
        , Layout.row
            [ Neat.text (String.fromInt model.tempSelection)
            ]
        , Layout.row
            (Array.toList
                model.unselectedGroup
                |> List.map String.fromInt
                |> List.map Neat.text
            )
        , Layout.row [ Neat.text <| Debug.toString model ]
        ]
        |> Neat.toPage
        |> div []



{-
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
-}
