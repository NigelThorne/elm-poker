port module Ports exposing (..)

import Json.Encode
import Data.Firebase exposing (FirebaseAction, FirebaseAction(..))


port signIn : () -> Cmd msg


port signOut : () -> Cmd msg

{-
  {
    command : string [onSnapshot, add]
    path: string,
    receivePort : portname,
    errorPort : portname,
  }
-}
port firebase : (FirebaseActionRecord) -> Cmd msg

type alias FirebaseActionRecord =
  {
    command : String,
    path : Maybe String,
    message : Maybe String, 
    receivePort : Maybe String,
    errorPort : Maybe String,
    unsubId : Maybe String
  }

toRecord : FirebaseAction -> FirebaseActionRecord
toRecord action = 
  case action of
    OnSnapshot data -> FirebaseActionRecord "read" (Just data.path) Nothing (Just data.receivePort) Nothing Nothing
    Unsub data -> FirebaseActionRecord "Unsub" Nothing Nothing Nothing Nothing (Just data.unsubId)
    Add data -> FirebaseActionRecord "Add" (Just data.path) (Just data.message) Nothing (Just data.errorPort) Nothing

port pokerRoomInfo : (Json.Encode.Value -> msg) -> Sub msg

port signInInfo : (Json.Encode.Value -> msg) -> Sub msg


port signInError : (Json.Encode.Value -> msg) -> Sub msg


port saveMessage : Json.Encode.Value -> Cmd msg


port receiveMessages : (Json.Encode.Value -> msg) -> Sub msg
