module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode exposing (Decoder, field, map6, string)



-- MODEL


type alias Model =
    { username : String
    , request : Request
    }


type Request
    = Waiting
    | Loading
    | Failure
    | Success User


type alias User =
    { name : String
    , avatar_url : String
    , company : String
    , blog : String
    , location : String
    , bio : String
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { username = "", request = Waiting }, Cmd.none )



-- UPDATE


type Msg
    = UsernameChanged String
    | UserRequested
    | UserReceived (Result Http.Error User)
    | CleanResults


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UsernameChanged newUsername ->
            ( { model | username = newUsername }, Cmd.none )

        UserRequested ->
            ( { model | request = Loading }, getGithubUser model.username )

        UserReceived result ->
            case result of
                Ok user ->
                    ( { model | request = Success user }, Cmd.none )

                Err _ ->
                    ( { model | request = Failure }, Cmd.none )
        
        CleanResults ->
            ( { username = "", request = Waiting }, Cmd.none )



-- SUBSCRIPTION


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    div [ class "application-wrapper" ]
        [ div [ class "header" ] [ h2 [] [ text "Github User" ] ]
        , div [ class "input-form" ]
            [ input [ placeholder "Digite o usuário", onInput UsernameChanged, value model.username ] []
            , button [ class "submit-button", onClick UserRequested ] [ text "Pesquisar" ]
            , button [ class "clean-button", onClick CleanResults ] [ text "Limpar" ]
            ]
        , div [ class "user-received" ] [ viewUser model.request ]
        ]


viewUser : Request -> Html Msg
viewUser request =
    case request of
        Waiting ->
            text "Digite um usuário para obter o perfil"

        Loading ->
            text "Carregando"

        Failure ->
            text "Ocorreu um erro :("

        Success user ->
            div []
                [ img [ src user.avatar_url ] []
                , p [] [ text ("Nome: " ++ user.name) ]
                , p [] [ text ("Empresa: " ++ user.company) ]
                , p [] [ text ("Localização: " ++ user.location) ]
                , p [] [ text ("Website: " ++ user.blog) ]
                , p [] [ text ("Bio: " ++ user.bio) ]
                ]



-- HTTTP


getGithubUser : String -> Cmd Msg
getGithubUser username =
    Http.get
        { url = "https://api.github.com/users/" ++ username
        , expect = Http.expectJson UserReceived userDecoder
        }


userDecoder : Decoder User
userDecoder =
    map6 User
        (field "name" string)
        (field "avatar_url" string)
        (field "company" string)
        (field "blog" string)
        (field "location" string)
        (field "bio" string)



-- MAIN

main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }
