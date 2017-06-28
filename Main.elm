module Main exposing (..)

import Mouse
import Html exposing (..)


--import Html.Attributes exposing (..)
--import Html.Events exposing (onClick)

import Plot exposing (..)


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { position : Mouse.Position }


initialModel : Model
initialModel =
    { position = Mouse.Position 0 0 }


init : ( Model, Cmd Msg )
init =
    ( initialModel, Cmd.none )



-- UPDATE


type Msg
    = MouseMove Mouse.Position


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        MouseMove position ->
            ( { model | position = position }, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Mouse.moves MouseMove



-- VIEW


view : Model -> Html Msg
view model =
    Plot.plot ( model.position.x, model.position.y )
