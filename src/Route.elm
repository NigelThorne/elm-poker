module Route exposing (..)


import Url.Parser exposing ((</>), Parser, int, map, oneOf, s, string, top)
import Html exposing (a)
import Url

type Route
    = Home 
    | NewGame
    | InGame String

toString : Maybe Route -> String
toString route = 
  Debug.toString route

fromUrl : Url.Url -> Maybe Route
fromUrl url = 
  Url.Parser.parse routeParser url

routeParser : Parser (Route -> a) a
routeParser =
    oneOf
        [ Url.Parser.map Home top
        , Url.Parser.map Home (s "home")
        , Url.Parser.map NewGame (s "poker" </> s "newgame")
        , Url.Parser.map InGame (s "poker" </> s "game" </> string)
        ]
