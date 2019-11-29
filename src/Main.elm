port module Main exposing (Model, Msg, init, subscriptions, update, view)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Encode
import Random


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Model =
    { rollstatus : Int
    , fullList : List Int
    , unselectedList : List Int
    , selectedList : List Int
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { rollstatus = 0
      , fullList = initializeList
      , unselectedList = initializeList
      , selectedList = []
      }
    , Cmd.none
    )


initializeList : List Int
initializeList =
    List.range 0 99


type Msg
    = Rollend
    | RandomGenerate Int
    | Rollstart Json.Encode.Value


port rollend : Int -> Cmd msg


selectValue : Cmd Msg
selectValue =
    Random.generate RandomGenerate (Random.int 0 99)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Rollend ->
            ( model, selectValue )

        Rollstart _ ->
            ( { model | rollstatus = 2 }, Cmd.none )

        RandomGenerate num ->
            ( { model
                | selectedList = model.selectedList ++ (List.drop num model.unselectedList |> List.take 1)
              }
            , Cmd.none
            )


port rollstart : (Json.Encode.Value -> msg) -> Sub msg


subscriptions : Model -> Sub Msg
subscriptions model =
    rollstart Rollstart


view : Model -> Html Msg
view model =
    div []
        [ button
            [ onClick Rollend ]
            [ text "とめる" ]
        , div []
            [ text <| "rollstatus:" ++ String.fromInt model.rollstatus ]
        , table [ style "border-collapse" "collapse" ]
            [ tr []
                (model.fullList
                    |> List.map String.fromInt
                    |> List.map text
                    |> List.map (\html -> td [ style "border" "1px solid" ] [ html ])
                )
            ]
        ]



{-
   separateDisplayList : List Int -> List List Int
   separateDisplayList orgList =
         kakuteiList = List.take 10 orgList
         newList = List.drop 10 orgList
-}
