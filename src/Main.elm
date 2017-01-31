module Main exposing (..)

import Html exposing (beginnerProgram, Html, div, text, form, input, button)
import Html.Events exposing (onSubmit, onInput, onClick)
import Html.Attributes exposing (value)


type alias State =
    { todo : String
    , todos : List String
    }


type alias States =
    { before : List State
    , now : State
    , after : List State
    }


type alias Model =
    { todo : String
    , todos : List String
    , states : States
    }


type Msg
    = ChangeTodo String
    | AddTodo String
    | Undo
    | Redo


view : Model -> Html Msg
view model =
    div []
        --[ div [] [ text (toString model) ]
        [ if List.length model.states.before > 0 then
            button [ onClick Undo ] [ text "Undo" ]
          else
            text ""
        , if List.length model.states.after > 0 then
            button [ onClick Redo ] [ text "Redo" ]
          else
            text ""
        , form
            [ onSubmit (AddTodo model.todo) ]
            [ input
                [ onInput ChangeTodo, value model.todo ]
                []
            , button [] [ text "Add" ]
            ]
        , div [] <| List.map todoView model.todos
        ]


todoView : String -> Html Msg
todoView todo =
    div [] [ text todo ]


update : Msg -> Model -> Model
update msg model =
    case msg of
        Undo ->
            let
                states =
                    States
                        (model.states.before
                            |> List.tail
                            |> Maybe.withDefault []
                        )
                        (model.states.before
                            |> List.head
                            |> Maybe.withDefault (State "" [])
                        )
                        (model.states.now :: model.states.after)
            in
                { model | todo = states.now.todo, todos = states.now.todos, states = states }

        Redo ->
            let
                states =
                    States
                        (model.states.now :: model.states.before)
                        (model.states.after
                            |> List.head
                            |> Maybe.withDefault (State "" [])
                        )
                        (model.states.after
                            |> List.tail
                            |> Maybe.withDefault []
                        )
            in
                { model | todo = states.now.todo, todos = states.now.todos, states = states }

        ChangeTodo todo ->
            let
                newModel =
                    { model | todo = todo }
            in
                updateStates model newModel

        AddTodo todo ->
            let
                newModel =
                    { model | todo = "", todos = model.todos ++ [ todo ] }
            in
                updateStates model newModel


updateStates : Model -> Model -> Model
updateStates oldModel newModel =
    let
        newStates =
            States
                (oldModel.states.now :: oldModel.states.before)
                (State newModel.todo newModel.todos)
                []
    in
        { newModel | states = newStates }


main : Program Never Model Msg
main =
    beginnerProgram
        { model = Model "" [] (States [] (State "" []) [])
        , view = view
        , update = update
        }
