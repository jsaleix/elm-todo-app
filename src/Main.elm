module Main exposing (..)

import Browser
import Browser.Dom
import Dict exposing (update)
import Html exposing (..)
import Html.Attributes exposing (class, style, type_, value, id, placeholder, autofocus, name)
import Html.Events exposing (onInput)
import Item
import Task

type alias Model = {
    tasks: List Item.Model
    , field: String
    , uuid: Int
    }

emptyModel : Model
emptyModel = {
    tasks = []
    , field = ""
    , uuid = 0
    }

-- Update part 

type Msg = 
    NoOp
    | UpdateField String
    | Add
    | UpdateItem (Int, Item.Msg)

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model = 
    case msg of
        NoOp -> ( model, Cmd.none )
        
        UpdateField value ->
            let 
                newModel = { model | field = value }
            in ( newModel, Cmd.none )

        Add ->
            let 
                description = String.trim model.field
                newModel = 
                    if description == "" then
                        model
                    else
                        { 
                            model | uuid = model.uuid + 1
                            , field = ""
                            , tasks = model.tasks ++ [ Item.init description model.uuid ]
                        }

            in ( newModel, Cmd.none )
        
        UpdateItem ( id, itemMsg ) ->
            let 
                updateItem t =
                    if t.id == id then
                        Item.update itemMsg t
                    else
                        Just t

                newModel = 
                    { model | tasks = List.filterMap updateItem model.tasks }
        
            in 
                case itemMsg of
                    Item.Focus elementId ->
                        ( newModel, focusTask elementId )
                    _ ->
                        ( newModel, Cmd.none )

-- View part

view: Model -> Html Msg
view model =
    div
        [ class "todomvc-wrapper"
        , style  "visibility" "visible"
        ]
        [ section
            [ class "todoapp" ]
            [ taskEntry model.field
            , taskList model.tasks
            ]
        ]
    
taskEntry : String -> Html Msg
taskEntry task =
    header
        [ class "header" ]
        [ h1 [] [ text "todos" ]
        , input
            [ class "new-todo"
            , placeholder "What needs to be done?"
            , autofocus True
            , value task
            , name "newTodo"
            , onInput UpdateField
            , Item.onFinish Add NoOp
            ]
            []
        ]
    
taskList : List Item.Model -> Html Msg
taskList tasks =
        section
            [ class "main"
            , style "visibility" "visible"
            ]
            [ input
                [ class "toggle-all"
                , id "toggle-all"
                , type_ "checkbox"
                , name "toggle"
                ]
                []
            , ul
                [ class "todo-list" ]
                (List.map
                    (\task ->
                        let
                            id =
                                task.id

                            taskView =
                                Item.view task
                        in
                            Html.map (\msg -> UpdateItem ( id, msg )) taskView
                    )
                    tasks
                )
            ]

focusTask : String -> Cmd Msg
focusTask elementId =
    Browser.Dom.focus elementId
    |> Task.attempt (\_ -> NoOp)

main : Program () Model Msg
main =
 Browser.element
    {
        init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
    }

init : () -> ( Model, Cmd Msg )
init _ =
    ( emptyModel
    , Cmd.none
    )

subscriptions : Model -> Sub Msg
subscriptions _ = 
    Sub.none