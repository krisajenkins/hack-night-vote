module Main exposing (..)
import Html.App as Html
import Html exposing (Html, text, div, input, ul, li)
import String
import Http
import Json.Decode as Json exposing ((:=))
import Task exposing (..)
import Platform.Cmd as Cmd exposing (Cmd)
import Http
import Date exposing (Date, Day)
import Result exposing (Result)


type alias Hack =
  { id: String
  , name: String
  }

type Msg
  = NoOp
  | UpdateHacks (List Hack)
  | FetchError Http.Error


type alias Model =
  { hacks: List Hack
  }


hackDecoder : Json.Decoder Hack
hackDecoder =
  Json.object2
    Hack
    ("id" := Json.string)
    ("name" := Json.string)


hacksDecoder : Json.Decoder (List Hack)
hacksDecoder = 
  Json.list hackDecoder


requestHacks : String -> Cmd Msg
requestHacks query =
  Http.get hacksDecoder "http://10.112.2.147.55:5000/api/hacks"
    |> Task.perform FetchError UpdateHacks


view : Model -> Html Msg
view model = div [] [ text "Hello world" ]


init : (Model, Cmd Msg)
init =
  ( { hacks = []
    }
  , Cmd.none
  )



update : Msg -> Model -> ( Model, Cmd Msg )
update _ model = ( model, Cmd.none ) 


subscriptions : Model -> Sub Msg
subscriptions _ = Sub.none

main =
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }
