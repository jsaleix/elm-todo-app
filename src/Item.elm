module Item exposing (..)

import Html exposing (..)
import Html.Attributes exposing (class, type_, value, id, placeholder, checked, name)
import Html.Events exposing (onInput, onClick, on, onBlur, keyCode, onDoubleClick)
import Json.Decode as Decode exposing (Decoder, field, int, list, string)
import Json.Decode.Pipeline exposing (required, optional)
-- Model

type alias Model =
    {
        description: String
        , completed: Bool
        , edits: Maybe String
        , id: String
    }

init : String -> String -> Model
init desc id = 
    {
        description = desc
        , completed = False
        , edits = Nothing
        , id = id
    }

emptyModel : Model
emptyModel = 
    {
        description = ""
        , completed = False
        , edits = Nothing
        , id = ""
    }

-- Update

type Msg = 
    Focus String
    | Edit String
    | Cancel
    | Commit
    | Completed Bool
    | Delete

update : Msg -> Model -> Maybe Model
update msg model =
    case msg of
        Focus _ -> Just { model | edits = Just model.description }
        
        Edit description -> 
            Just { model | edits = Just description }
        
        Cancel ->
            Just { model | edits = Nothing }
        
        Commit -> 
            case model.edits of
                Nothing -> 
                    Just model
                
                Just rawDescription ->
                    let 
                        description = String.trim rawDescription
                    in
                        if String.isEmpty description then 
                            Nothing
                        else
                            Just { model | description = description, edits = Nothing }

        Completed bool -> Just{ model | completed = bool }

        Delete -> Nothing

-- View

view : Model -> Html Msg
view model =
    let
        className =
            if model.completed then
                "completed"
            else
                ""
            ++ case model.edits of
                Just _ -> 
                    " editing"
                Nothing -> 
                    ""

        description =
            Maybe.withDefault model.description model.edits
        elementId =
            "todo-" ++ model.id
    in
        li
            [ class className ]
            [ div
                [ class "view" ]
                [ input
                    [ class "toggle"
                    , type_ "checkbox"
                    , checked model.completed
                    , onClick (Completed (not model.completed))
                    ]
                    []
                , label 
                    [ onDoubleClick (Focus elementId) ]
                    [ text description ]
                , button 
                    [ class "destroy"
                    , onClick Delete
                    ]
                    []
                ]
            , input
                [ class "edit"
                , id elementId
                , value description
                , placeholder "What needs to be done?"
                , name "title"
                , onInput Edit
                , onBlur Commit
                , onFinish Commit Cancel
                ]
                []
            ]

onFinish : msg -> msg -> Attribute msg
onFinish finishMsg keyMsg =
    let
        select key =
            case key of
                13 -> finishMsg
                _ -> keyMsg
    in
        on "keydown" (Decode.map select keyCode)


itemDecoder : Decoder Model
itemDecoder =
    Decode.succeed Model
        |> required "_description" string
        |> required "_completed" Decode.bool
        |> optional "edits" (Decode.maybe string) Nothing
        |> required "id" string

itemListDecoder : Decoder (List Model)
itemListDecoder =
    (list itemDecoder)
    
