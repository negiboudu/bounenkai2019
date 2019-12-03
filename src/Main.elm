port module Main exposing (Model, Msg(..), Rollstatus(..), class, createGroup, creatediv, getClassName, getSelection, getText, init, lampPadding, main, monitorPadding, rollend, rollstart, subscriptions, update, view)

import Array exposing (Array)
import Browser
import Browser.Events
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Attributes.Classname exposing (classMixinWith)
import Html.Events exposing (..)
import Json.Encode
import Mixin exposing (Mixin)
import Neat exposing (..)
import Neat.Layout as Layout
import Neat.Layout.Column as Column exposing (..)
import Neat.Layout.Row as Row exposing (..)
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
    , tempSelection : String
    , fullGroup : List String
    , unselectedGroup : Array String
    , selectedGroup : Array String
    , animationCount : Int
    , animationInterval : Int
    , animationIntervalLimit : Int
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { rollstatus = Stop
      , tempSelection = "?"
      , fullGroup = Array.toList createGroup
      , unselectedGroup = createGroup
      , selectedGroup = Array.empty
      , animationCount = 0
      , animationInterval = 0
      , animationIntervalLimit = 20
      }
    , Cmd.none
    )


createGroup : Array String
createGroup =
    Array.initialize 100 identity
        |> Array.map String.fromInt
        |> Array.push "SP"


type Msg
    = RandomGenerate Int
    | Rollstart Json.Encode.Value
    | ComeonAnimationFrame Time.Posix


port rollend : Int -> Cmd msg


getSelection : Array String -> Int -> String
getSelection arr index =
    case Array.get index arr of
        Just selection ->
            selection

        Nothing ->
            "?"


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
    Layout.columnWith
        { defaultColumn
            | horizontal = Column.HCenter
        }
        [ Neat.div
            [ class "monitor" ]
            [ Neat.text <| model.tempSelection ]
            |> fromNoPadding monitorPadding
        , Layout.rowWith
            { defaultRow
                | vertical = Row.VCenter
                , horizontal = Row.HCenter
                , wrap = True
            }
            (model.fullGroup
                |> List.map (creatediv model)
            )
        , lift button [] [ Neat.text "まわす" ]
            |> setMixin (Mixin.fromAttribute (onClick <| Rollstart (Json.Encode.int 0)))
        , Neat.text <| Debug.toString model.rollstatus
        ]
        |> toPage
        |> Html.div []


creatediv : Model -> String -> View p Msg
creatediv model num =
    Neat.div (getClassName model num) (getText num)


getClassName : Model -> String -> List (Mixin Msg)
getClassName model num =
    if Array.toList model.selectedGroup |> List.member num then
        [ class "selected", class "listitem" ]

    else if model.tempSelection == num then
        [ class "tempSelection", class "listitem" ]

    else
        [ class "unselected", class "listitem" ]


getText : String -> List (View p Msg)
getText num =
    Neat.text num
        |> fromNoPadding lampPadding
        |> (\v -> [ v ])



-- paddings


monitorPadding : IsPadding p
monitorPadding =
    IsPadding { rem = 0.1 }


lampPadding : IsPadding p
lampPadding =
    IsPadding { rem = 0.5 }



-- Helper functions


class : String -> Mixin msg
class =
    classMixinWith <| \name -> "app__" ++ name
