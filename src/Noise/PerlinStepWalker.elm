module Noise.PerlinStepWalker exposing (..)

import Browser
import Color
import Html exposing (Html)
import Time
import TypedSvg as Svg
import TypedSvg.Core exposing (Svg)
import TypedSvg.Attributes as Attributes
import TypedSvg.Types exposing (Fill(..), px)
import Random
import Noise.SimplexNoise as Noise
import Utils

type alias Model =
  { positions : List Position
  , time : (Float, Float)
  }

type alias Position =
  (Float, Float)

type Msg
  = NewLength Time.Posix

main : Program () Model Msg
main =
  Browser.element
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }


permutationTable =
  Tuple.first <| Noise.permutationTable (Random.initialSeed 42)


newPosition (tx, ty) oldPosition =
  let
    (oldX, oldY) =
      oldPosition
    generateStep time =
      Utils.lerp -1 1 -2 2 <| Noise.noise1d permutationTable time
  in
  (oldX + generateStep tx, oldY + generateStep ty)

init : () -> (Model, Cmd Msg)
init _ =
  ( { positions = []
  , time = (0, 10000)
  }
  , Cmd.none
  )
  

view : Model -> Html Msg
view model =
  Svg.svg
    [ Attributes.width (px 600)
    , Attributes.height (px 600)
    , Attributes.viewBox 0 0 600 600
    ] <|
    List.map
      point
      model.positions


point : Position -> Svg Msg
point position =
  let
    (x, y) =
      position
  in
  Svg.circle
    [ Attributes.cx (px x)
    , Attributes.cy (px y)
    , Attributes.r (px 1)
    , Attributes.fill <| Fill Color.black
    ]
    []

subscriptions : Model -> Sub Msg
subscriptions _ =
  Sub.batch
    [ Time.every 10 NewLength
    ]

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    NewLength _ ->
      let
        (tx, ty) =
          model.time
        newTime =
          (tx + 0.01, ty + 0.01)
        oldPosition =
          Maybe.withDefault (300, 300) <| List.head model.positions
      in
      ( { model |
        time = newTime
        , positions =
          newPosition newTime oldPosition :: model.positions
      }
      , Cmd.none
      )
