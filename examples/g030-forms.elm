module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import Regex exposing (contains, regex)


main =
    Html.beginnerProgram
        { model = model
        , view = view
        , update = update
        }



-- Model


type alias Model =
    { name : String
    , password : String
    , passwordConfirmation : String
    , age : String
    }


model : Model
model =
    Model "" "" "" "0"



--- Update


type Msg
    = Name String
    | Password String
    | PasswordConfirmation String
    | Age String


update : Msg -> Model -> Model
update msg model =
    case msg of
        Name name ->
            { model | name = name }

        Password password ->
            { model | password = password }

        PasswordConfirmation passwordConfirmation ->
            { model | passwordConfirmation = passwordConfirmation }

        Age age ->
            { model | age = age }



-- View


view : Model -> Html Msg
view model =
    div []
        [ input [ type_ "text", placeholder "Name", onInput Name ] []
        , input [ type_ "password", placeholder "Password", onInput Password ] []
        , input [ type_ "password", placeholder "Re-enter Password", onInput PasswordConfirmation ] []
        , input [ type_ "number", placeholder "Age", onInput Age ] []
        , viewValidation model
        ]


viewValidation : Model -> Html msg
viewValidation model =
    let
        ( color, message ) =
            if Regex.contains (Regex.regex "^(?=.*?[A-Z])(?=.*?[a-z])(?=.*?[0-9])(?=.*?[#?!@$%^&*-]).{8,}$") model.password == False then
                ( "red", "Password must have more than 8, upper, lower and special chars" )
            else if model.password == model.passwordConfirmation then
                ( "red", "Age must be a number" )
            else if model.password == model.passwordConfirmation then
                ( "green", "OK" )
            else
                ( "red", "Passwords do not match!" )
    in
        div [ style [ ( "color", color ) ] ] [ text message ]
