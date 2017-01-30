module Main exposing (..)

import Html exposing (beginnerProgram, div, text, form, input, button)
import Html.Events exposing (onSubmit, onInput, onClick)
import Html.Attributes exposing (value)


type alias State =
    { todo : String, todos : List String }


type alias Model =
    { todo : String, todos : List String, states : List State }


type Msg
    = ChangeTodo String
    | AddTodo String
    | Undo


view model =
    div []
        [ button [ onClick Undo ] [ text "Undo" ]
        , form
            [ onSubmit (AddTodo model.todo) ]
            [ input
                [ onInput ChangeTodo, value model.todo ]
                []
            , button [] [ text "Add" ]
            ]
        , div [] <| List.map todoView model.todos
        ]


todoView todo =
    div [] [ text todo ]


update msg modelBefore =
    let
        states =
            modelBefore.states ++ [ { todo = modelBefore.todo, todos = modelBefore.todos } ]

        model =
            { modelBefore | states = states }
    in
        case msg of
            ChangeTodo todo ->
                { model | todo = todo }

            AddTodo todo ->
                { model | todos = model.todos ++ [ todo ], todo = "" }

            Undo ->
                let
                    lastState =
                        modelBefore.states
                            |> List.reverse
                            |> List.head
                            |> Maybe.withDefault (State "" [])

                    lastStateRemoved =
                        modelBefore.states
                            |> List.reverse
                            |> List.tail
                            |> Maybe.withDefault [ State "" [] ]
                            |> List.reverse
                in
                    { todo = lastState.todo
                    , todos = lastState.todos
                    , states = lastStateRemoved
                    }


main =
    beginnerProgram
        { model = Model "" [] []
        , view = view
        , update = update
        }
