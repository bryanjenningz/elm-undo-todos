module Main exposing (..)

import Html exposing (program, Html, div, text, form, input, button, span)
import Html.Events exposing (onSubmit, onInput, onClick)
import Html.Attributes exposing (value, class, style, id, autocomplete)
import Dom
import Task


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
    = NoOp
    | ChangeTodo String
    | AddTodo String
    | RemoveTodo Int
    | Undo
    | Redo


view : Model -> Html Msg
view model =
    div [ class "col-sm-4 col-sm-offset-4" ]
        [ if List.length model.states.before > 0 then
            div
                [ class <|
                    if List.length model.states.after > 0 then
                        "col-xs-6"
                    else
                        "col-xs-12"
                , style [ ( "padding", "0" ) ]
                ]
                [ button [ onClick Undo, class "btn btn-default form-control" ] [ text "Undo" ] ]
          else
            text ""
        , if List.length model.states.after > 0 then
            div
                [ class <|
                    if List.length model.states.before > 0 then
                        "col-xs-6"
                    else
                        "col-xs-12"
                , style [ ( "padding", "0" ) ]
                ]
                [ button [ onClick Redo, class "btn btn-default form-control" ] [ text "Redo" ] ]
          else
            text ""
        , form
            [ onSubmit (AddTodo model.todo) ]
            [ div [ class "row", style [ ( "margin", "0" ) ] ]
                [ div [ class "col-xs-10", style [ ( "padding", "0" ) ] ]
                    [ input
                        [ onInput ChangeTodo, value model.todo, class "form-control", id "input-box", autocomplete False ]
                        []
                    ]
                , div [ class "col-xs-2", style [ ( "padding", "0" ) ] ]
                    [ button [ class "btn btn-primary form-control" ] [ text "Add" ] ]
                ]
            ]
        , todosView model.todos
        ]


todosView : List String -> Html Msg
todosView todos =
    div [ class "list-group" ] <|
        List.indexedMap
            (\i todo ->
                div [ class "list-group-item" ]
                    [ text todo
                    , span [ class "pull-right glyphicon glyphicon-remove", onClick (RemoveTodo i) ] []
                    ]
            )
            todos


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            model ! []

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
                { model | todo = states.now.todo, todos = states.now.todos, states = states } ! []

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
                { model | todo = states.now.todo, todos = states.now.todos, states = states } ! []

        ChangeTodo todo ->
            let
                newModel =
                    { model | todo = todo }
            in
                updateStates model newModel ! []

        AddTodo todo ->
            let
                newModel =
                    { model | todo = "", todos = model.todos ++ [ todo ] }
            in
                ( updateStates model newModel, focusInputBox )

        RemoveTodo index ->
            let
                front =
                    List.take index model.todos

                back =
                    List.drop (index + 1) model.todos

                newModel =
                    { model | todos = front ++ back }
            in
                updateStates model newModel ! []


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


focusInputBox : Cmd Msg
focusInputBox =
    Task.attempt (always NoOp) (Dom.focus "input-box")


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


main =
    program
        { init = ( Model "" [] (States [] (State "" []) []), focusInputBox )
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
