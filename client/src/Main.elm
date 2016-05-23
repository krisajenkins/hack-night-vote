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
  }

type Msg
  = NoOp
  | UpdateHacks (List Hack)
  | RequestUpdateHacks
  | FetchError Http.Error


type alias Model =
  { hacks: List Hack
  }


hackDecoder : Json.Decoder Hack
hackDecoder =
  Json.object2
    Hack
    ("id" := Json.int)
    ("name" := Json.string)


hacksDecoder : Json.Decoder (List Hack)
hacksDecoder = 
  Json.list hackDecoder


requestHacks : Cmd Msg
requestHacks =
  Http.get hacksDecoder "http://10.112.147.55:5000/api/hacks"
    |> Task.perform FetchError UpdateHacks


renderHack : Hack -> Html a
renderHack hack =
  li [] [text <| toString hack.id, text " ", text hack.name]


view : Model -> Html Msg
view model = div []
    [ button
        [ type' "button"
        , onClick RequestUpdateHacks
        ]
        [ text "refresh" ]
    , ul []
      (List.map renderHack model.hacks)
    ]


init : (Model, Cmd Msg)
init =
  ( { hacks = []
    }
  , Cmd.none
  )

update : Msg -> Model -> ( Model, Cmd Msg )
update action model =
  case action of
    RequestUpdateHacks ->
      ( model, requestHacks)
    UpdateHacks hacks ->
      ( { model | hacks = hacks }, Cmd.none ) 
    FetchError error ->
      Debug.log (toString error)
      ( model, Cmd.none ) 
    NoOp ->
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
