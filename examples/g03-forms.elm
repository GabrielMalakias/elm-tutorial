module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick)
import Regex exposing (contains, regex)
import Char exposing (isDigit)


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
    , result : ValidationResult
    }


model : Model
model =
    Model "" "" "" "0" NotDone



--- Update


type ValidationResult
    = Ok
    | PasswordMustHaveUpperLowerAndSpecialError
    | AgeMustBeANumberError
    | PasswordsDoesntMatchError
    | NotDone


type Msg
    = Name String
    | Password String
    | PasswordConfirmation String
    | Age String
    | Submit


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

        Submit ->
            { model | result = validateModel model }



-- View


view : Model -> Html Msg
view model =
    div []
        [ input [ type_ "text", placeholder "Name", onInput Name ] []
        , input [ type_ "password", placeholder "Password", onInput Password ] []
        , input [ type_ "password", placeholder "Re-enter Password", onInput PasswordConfirmation ] []
        , input [ type_ "number", placeholder "Age", onInput Age ] []
        , button [ onClick Submit ] [ text "test" ]
        , viewValidation model
        ]


validateModel : Model -> ValidationResult
validateModel model =
    if Regex.contains (Regex.regex "^(?=.*?[A-Z])(?=.*?[a-z])(?=.*?[0-9])(?=.*?[#?!@$%^&*-]).{8,}$") model.password == False then
        PasswordMustHaveUpperLowerAndSpecialError
    else if Result.withDefault -1 (String.toInt model.age) == -1 then
        AgeMustBeANumberError
    else if model.password == model.passwordConfirmation then
        Ok
    else
        PasswordsDoesntMatchError


viewValidation : Model -> Html Msg
viewValidation model =
    let
        ( color, message ) =
            case model.result of
                NotDone ->
                    ( "", "" )

                AgeMustBeANumberError ->
                    ( "red", "Age must be a number" )

                PasswordMustHaveUpperLowerAndSpecialError ->
                    ( "red", "Password must have more than 8, upper, lower and special chars" )

                Ok ->
                    ( "green", "OK" )

                PasswordsDoesntMatchError ->
                    ( "red", "Passwords do not match!" )
    in
        div [ style [ ( "color", color ) ] ] [ text message ]
