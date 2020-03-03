module Main exposing (..)

import Browser
import Color
import Element as El exposing (column, el, fill, height, pointer, row, spacing, text, width)
import Element.Events exposing (onClick)
import Html exposing (Html)
import RandomWalks.Basic2 as BasicWalker
import TypedSvg as Svg
import TypedSvg.Attributes as Attributes
import TypedSvg.Types exposing (px)
import Vector.BouncingBall2 as BouncingBall



-- INIT


init : () -> ( Model, Cmd Msg )
init _ =
    let
        ( subModel, subCmd ) =
            BasicWalker.init
    in
    ( BasicWalkerAnim subModel
    , subCmd
        |> Cmd.map BasicWalkerMsg
    )



-- MODEL


type Model
    = BasicWalkerAnim BasicWalker.Model
    | BouncingBallAnim BouncingBall.Model


type Animation
    = BasicWalker
    | BouncingBall



-- UPDATE


type Msg
    = Select Animation
    | BasicWalkerMsg BasicWalker.Msg
    | BouncingBallMsg BouncingBall.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
        ( Select anim, _ ) ->
            case anim of
                BasicWalker ->
                    let
                        ( subModel, subCmd ) =
                            BasicWalker.init
                    in
                    ( BasicWalkerAnim subModel
                    , subCmd
                        |> Cmd.map BasicWalkerMsg
                    )

                BouncingBall ->
                    ( BouncingBallAnim
                        BouncingBall.init
                    , Cmd.none
                    )

        ( BasicWalkerMsg subMsg, BasicWalkerAnim subModel ) ->
            let
                ( newSubModel, subCmd ) =
                    subModel
                        |> BasicWalker.update subMsg
            in
            ( BasicWalkerAnim newSubModel
            , subCmd
                |> Cmd.map BasicWalkerMsg
            )

        ( BouncingBallMsg subMsg, BouncingBallAnim subModel ) ->
            ( BouncingBallAnim
                (subModel
                    |> BouncingBall.update subMsg
                )
            , Cmd.none
            )

        ( _, _ ) ->
            ( model, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions anim =
    case anim of
        BasicWalkerAnim _ ->
            BasicWalker.subscriptions
                |> Sub.map BasicWalkerMsg

        BouncingBallAnim _ ->
            BouncingBall.subscriptions
                |> Sub.map BouncingBallMsg



-- VIEW


defaultWidth : Float
defaultWidth =
    600


defaultHeight : Float
defaultHeight =
    600


view : Model -> Html Msg
view model =
    El.layout
        [ width fill
        , height fill
        ]
        (row
            [ width fill
            , height fill
            ]
            [ column
                [ width fill
                , height fill
                ]
                [ model
                    |> demoView
                    |> El.html
                ]
            , column
                [ width fill
                , height fill
                , spacing 20
                ]
                [ el
                    [ onClick (Select BasicWalker)
                    , pointer
                    ]
                    (text "Basic Walker")
                , el
                    [ onClick (Select BouncingBall)
                    , pointer
                    ]
                    (text "Bouncing Ball")
                ]
            ]
        )


demoView : Model -> Html Msg
demoView model =
    Svg.svg
        [ Attributes.width (px defaultWidth)
        , Attributes.height (px defaultHeight)
        , Attributes.viewBox 0 0 defaultWidth defaultHeight
        ]
    <|
        [ border
        , case model of
            BasicWalkerAnim subModel ->
                BasicWalker.view subModel.positions
                    |> Html.map BasicWalkerMsg

            BouncingBallAnim subModel ->
                BouncingBall.view subModel.ballRadius subModel.position
                    |> Html.map BouncingBallMsg
        ]


border =
    Svg.rect
        [ Attributes.width (px defaultWidth)
        , Attributes.height (px defaultHeight)
        , Attributes.noFill
        , Attributes.stroke Color.black
        , Attributes.strokeWidth (px 3)
        ]
        []



-- MAIN


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
