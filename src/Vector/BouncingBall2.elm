module Vector.BouncingBall2 exposing (..)

import Browser
import Color
import Time
import TypedSvg as Svg
import TypedSvg.Attributes as Attributes
import TypedSvg.Core exposing (Svg)
import TypedSvg.Types exposing (Fill(..), px)



-- INIT


init : Model
init =
    { position = defaultPosition
    , xSpeed = 3
    , ySpeed = -1
    , ballRadius = 20
    }



-- MODEL


type alias Position =
    ( Float, Float )


type alias Model =
    { position : Position
    , xSpeed : Float
    , ySpeed : Float
    , ballRadius : Float
    }



-- UPDATE


type Msg
    = Move Time.Posix


update : Msg -> Model -> Model
update msg model =
    let
        ( x, y ) =
            model.position

        r =
            model.ballRadius

        xSpeed =
            model.xSpeed

        newX =
            x + xSpeed

        newXSpeed =
            if newX <= r || newX >= width - r then
                -1 * xSpeed

            else
                xSpeed

        ySpeed =
            model.ySpeed

        newY =
            y + ySpeed

        newYSpeed =
            if newY <= r || newY >= height - r then
                -1 * ySpeed

            else
                ySpeed
    in
    case msg of
        Move _ ->
            { model
                | xSpeed =
                    newXSpeed
                , ySpeed =
                    newYSpeed
                , position =
                    ( newX, newY )
            }



-- SUBSCRIPTIONS


subscriptions : Sub Msg
subscriptions =
    Time.every 10 Move



-- VIEW


defaultPosition : Position
defaultPosition =
    ( 300, 300 )


width : Float
width =
    600


height : Float
height =
    600


view : Float -> Position -> Svg Msg
view radius position =
    let
        ( x, y ) =
            position
    in
    Svg.circle
        [ Attributes.cx (px x)
        , Attributes.cy (px y)
        , Attributes.r (px radius)
        , Attributes.fill <| Fill Color.black
        ]
        []
