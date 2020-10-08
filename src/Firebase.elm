module Firebase exposing (Model, Msg(..), signInError, errorPrinter, init, isSignedIn, messageEncoder, setError, signInInfo, update)

import Json.Decode
import Json.Decode.Pipeline
import Json.Encode


type alias Model =
    { userData : Maybe UserData
    , error : ErrorData
    , signIn : () -> Cmd Msg
    , signOut : () -> Cmd Msg
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



--init : Model


init {signIn, signOut} =
    { userData = Maybe.Nothing, error = emptyError, signIn = signIn, signOut = signOut }


isSignedIn : Model -> Bool
isSignedIn model =
    case model.userData of
        Nothing ->
            False

        _ ->
            True


emptyError : ErrorData
emptyError =
    { code = Maybe.Nothing, credential = Maybe.Nothing, message = Maybe.Nothing }


type Msg
    = LogIn
    | LogOut
    | LoggedInData (Result Json.Decode.Error UserData)
    | LoggedInError (Result Json.Decode.Error ErrorData)


setError : Model -> Json.Decode.Error -> Model
setError model error =
    { model | error = messageToError <| Json.Decode.errorToString error }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LogIn ->
            ( model, model.signIn () )

        LogOut ->
            ( { model | userData = Maybe.Nothing, error = emptyError }, model.signOut () )

        LoggedInData result ->
            case result of
                Ok value ->
                    ( { model | userData = Just value }, Cmd.none )

                Err error ->
                    ( { model | error = messageToError <| Json.Decode.errorToString error }, Cmd.none )

        LoggedInError result ->
            case result of
                Ok value ->
                    ( { model | error = value }, Cmd.none )

                Err error ->
                    ( { model | error = messageToError <| Json.Decode.errorToString error }, Cmd.none )


messageToError : String -> ErrorData
messageToError message =
    { code = Maybe.Nothing, credential = Maybe.Nothing, message = Just message }


messageEncoder : Model -> String -> Json.Encode.Value
messageEncoder model message =
    Json.Encode.object
        [ ( "content", Json.Encode.string message )
        , ( "uid"
          , case model.userData of
                Just userData ->
                    Json.Encode.string userData.uid

                Maybe.Nothing ->
                    Json.Encode.null
          )
        ]


errorPrinter : ErrorData -> String
errorPrinter errorData =
    Maybe.withDefault "" errorData.code ++ " " ++ Maybe.withDefault "" errorData.credential ++ " " ++ Maybe.withDefault "" errorData.message


userDataDecoder : Json.Decode.Decoder UserData
userDataDecoder =
    Json.Decode.succeed UserData
        |> Json.Decode.Pipeline.required "token" Json.Decode.string
        |> Json.Decode.Pipeline.required "email" Json.Decode.string
        |> Json.Decode.Pipeline.required "uid" Json.Decode.string


logInErrorDecoder : Json.Decode.Decoder ErrorData
logInErrorDecoder =
    Json.Decode.succeed ErrorData
        |> Json.Decode.Pipeline.required "code" (Json.Decode.nullable Json.Decode.string)
        |> Json.Decode.Pipeline.required "message" (Json.Decode.nullable Json.Decode.string)
        |> Json.Decode.Pipeline.required "credential" (Json.Decode.nullable Json.Decode.string)


signInInfo : Json.Encode.Value -> Msg
signInInfo =
    Json.Decode.decodeValue userDataDecoder >> LoggedInData
signInError : Json.Encode.Value -> Msg
signInError =
    Json.Decode.decodeValue logInErrorDecoder >> LoggedInError
