port module Firebase exposing (..)

import Json.Decode
import Json.Decode.Pipeline
import Json.Encode


port signIn : () -> Cmd msg
port signOut : () -> Cmd msg
port signInInfo : (Json.Encode.Value -> msg) -> Sub msg
port signInError : (Json.Encode.Value -> msg) -> Sub msg


type alias FirebaseModel =
    { userData : Maybe UserData
    , error : ErrorData
    }

type alias ErrorData =
    { code : Maybe String
    , message : Maybe String
    , credential : Maybe String
    }


type alias UserData =
    { token : String
    , email : String
    , uid : String
    }


initFirebase : FirebaseModel
initFirebase =
    { userData = Maybe.Nothing, error = emptyError }


isSignedIn : FirebaseModel -> Bool
isSignedIn model =
    case model.userData of
        Nothing ->
            False

        _ ->
            True




emptyError : ErrorData
emptyError =
    { code = Maybe.Nothing, credential = Maybe.Nothing, message = Maybe.Nothing }


type FirebaseMsg
    = LogIn
    | LogOut
    | LoggedInData (Result Json.Decode.Error UserData)
    | LoggedInError (Result Json.Decode.Error ErrorData)

setError : FirebaseModel -> ErrorData -> FirebaseModel
setError model error =
    { model | error = error }

updateFirebase : FirebaseMsg -> FirebaseModel -> (FirebaseModel ,Cmd msg)
updateFirebase msg firebase =
    case msg of
        LogIn ->
            ( firebase, signIn () )

        LogOut ->
            ( { firebase | userData = Maybe.Nothing, error = emptyError } , signOut () )

        LoggedInData result ->
            case result of
                Ok value ->
                    ( { firebase | userData = Just value } , Cmd.none )

                Err error ->
                    ( { firebase | error = messageToError <| Json.Decode.errorToString error } , Cmd.none )

        LoggedInError result ->
            case result of
                Ok value ->
                    ( { firebase | error = value } , Cmd.none )

                Err error ->
                    ( { firebase | error = messageToError <| Json.Decode.errorToString error } , Cmd.none )


messageToError : String -> ErrorData
messageToError message =
    { code = Maybe.Nothing, credential = Maybe.Nothing, message = Just message }

