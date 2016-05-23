module Main exposing (..)
import Html.App as Html
import Html exposing (Html, text, div, input, button, ul, li)
import Html.Attributes exposing (type')
import Html.Events exposing (onClick)
import String
import Http
import Json.Decode as Json exposing ((:=))
import Task exposing (..)
import Platform.Cmd as Cmd exposing (Cmd)
import Http
import Date exposing (Date, Day)
import Result exposing (Result)


type alias Hack =
  { id: Int
  , name: String
  , voteCount: Int
  }

type alias Hacker =
  { hackerId: String
  , hackerName: String
  }

type Msg
  = NoOp

  | Refresh

  | UpdateHacks (List Hack)
  | UpdateHackers (List Hacker)

  | RequestSetHacker
  | SetHacker Int

  | FetchError Http.Error


type alias Model =
  { hacks: List Hack
  , hackers: List Hacker
  , currentHacker: Maybe Int
  }


hackDecoder : Json.Decoder Hack
hackDecoder =
  Json.object3
    Hack
    ("id" := Json.int)
    ("name" := Json.string)
    ("voteCount" := Json.int)


hacksDecoder : Json.Decoder (List Hack)
hacksDecoder =
  Json.list hackDecoder


hackerDecoder : Json.Decoder Hacker
hackerDecoder =
  Json.object2
    Hacker
    ("hackerId" := Json.string)
    ("hackerName" := Json.string)


hackersDecoder : Json.Decoder (List Hacker)
hackersDecoder =
  Json.list hackerDecoder


requestHacks : Cmd Msg
requestHacks =
  Http.get hacksDecoder "http://10.112.147.55:5000/api/hacks"
    |> Task.perform FetchError UpdateHacks


requestHackers : Cmd Msg
requestHackers =
  Http.get hackersDecoder "http://10.112.147.55:5000/api/hackers"
    |> Task.perform FetchError UpdateHackers


renderHack : Hack -> Html a
renderHack hack =
  li []
    [ text <| toString hack.id
    , text ": "
    , text hack.name
    , text " (", text <| toString hack.voteCount, text ")"
    ]

renderHackers : Hacker -> Html a
renderHackers hacker =
  li [] [text hacker.hackerId, text ": ", text hacker.hackerName]

view : Model -> Html Msg
view model = div []
    [ button
        [ type' "button"
        , onClick Refresh
        ]
        [ text "refresh" ]
    , ul []
      (List.map renderHack model.hacks)
    , ul []
      (List.map renderHackers model.hackers)
    ]

refresh : Cmd Msg
refresh = Cmd.batch [requestHacks, requestHackers]

init : (Model, Cmd Msg)
init =
  ( { hacks = []
    , hackers = []
    , currentHacker = Nothing
    }
  , refresh
  )

update : Msg -> Model -> ( Model, Cmd Msg )
update action model =
  case action of
    UpdateHacks hacks ->
      ( { model | hacks = hacks }, Cmd.none )
    UpdateHackers hackers ->
      ( { model | hackers = hackers }, Cmd.none )
    Refresh ->
      ( model, refresh )
    FetchError error ->
      Debug.log (toString error)
      ( model, Cmd.none )
    _ ->
      ( model, Cmd.none )



subscriptions : Model -> Sub Msg
subscriptions _ = Sub.none

main =
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }
