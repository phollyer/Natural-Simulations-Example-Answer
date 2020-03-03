module RandomWalks.Basic2 exposing (..)

import Browser
import Color
import Html exposing (Html)
import Random
import Time
import TypedSvg as Svg
import TypedSvg.Attributes as Attributes
import TypedSvg.Core exposing (Svg)
import TypedSvg.Types exposing (Fill(..), px)



-- INIT


init : ( Model, Cmd Msg )
init =
    ( { positions = [ defaultPosition ]
      }
    , stepCmd
    )


type alias Position =
    ( Float, Float )


defaultPosition : Position
defaultPosition =
    ( 300, 300 )


type Step
    = Up
    | Down
    | Left
    | Right


stepCmd : Cmd Msg
stepCmd =
    Random.generate NewStep <| Random.uniform Up [ Down, Left, Right ]



-- MODEL


type alias Model =
    { positions : List Position
    }



-- UPDATE


type Msg
    = GetStep Time.Posix
    | NewStep Step


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GetStep _ ->
            ( model
            , stepCmd
            )

        NewStep step ->
            let
                ( x, y ) =
                    Maybe.withDefault defaultPosition <| List.head model.positions

                delta =
                    6

                newPosition =
                    case step of
                        Up ->
                            ( x, y + delta )

                        Down ->
                            ( x, y - delta )

                        Left ->
                            ( x - delta, y )

                        Right ->
                            ( x + delta, y )
            in
            ( { model
                | positions =
                    newPosition :: model.positions
              }
            , Cmd.none
            )



-- SUBSCRIPTIONS


subscriptions : Sub Msg
subscriptions =
    Time.every 10 GetStep



-- VIEW


view : List Position -> Html Msg
view positions =
    Svg.svg
        [ Attributes.width (px 600)
        , Attributes.height (px 600)
        , Attributes.viewBox 0 0 600 600
        ]
    <|
        List.map
            point
            positions


point : Position -> Svg Msg
point position =
    let
        ( x, y ) =
            position
    in
    Svg.circle
        [ Attributes.cx (px x)
        , Attributes.cy (px y)
        , Attributes.r (px 3)
        , Attributes.fill <| Fill Color.black
        ]
        []
