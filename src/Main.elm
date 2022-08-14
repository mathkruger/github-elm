module Main exposing (..)

import Browser
import Html exposing (Html, a, button, div, figure, h3, img, input, li, p, strong, text, ul)
import Html.Attributes exposing (class, href, placeholder, src, value)
import Html.Events exposing (onClick, onInput)
import Http
import Json.Decode as Decode exposing (Decoder, int, string)
import Json.Decode.Pipeline exposing (optional, required)



-- MODEL


type alias Model =
    { username : String
    , user : Request User
    , repos : Request (List Repo)
    }


type Request a
    = Waiting
    | Loading
    | Failure Http.Error
    | Success a


type alias User =
    { login : String
    , name : String
    , avatarUrl : String
    , company : String
    , blog : String
    , location : String
    , bio : String
    }


type alias Repo =
    { name : String
    , fullName : String
    , htmlUrl : String
    , description : String
    , stars : Int
    , watchers : Int
    , forks : Int
    , language : String
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { username = "", user = Waiting, repos = Waiting }, Cmd.none )



-- UPDATE


type Msg
    = UsernameChanged String
    | UserRequested
    | UserReceived (Result Http.Error User)
    | ReposReceived (Result Http.Error (List Repo))
    | CleanResults


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UsernameChanged newUsername ->
            ( { model | username = newUsername }, Cmd.none )

        -- Requesting the user
        UserRequested ->
            ( { model | user = Loading }, getGithubUser model.username )

        UserReceived result ->
            case result of
                Ok user ->
                    ( { model | user = Success user, repos = Loading }, getUserRepos model.username )

                Err err ->
                    ( { model | user = Failure err }, Cmd.none )

        -- Requesting the user repos
        ReposReceived result ->
            case result of
                Ok repos ->
                    ( { model | repos = Success repos }, Cmd.none )

                Err err ->
                    ( { model | repos = Failure err }, Cmd.none )

        -- Cleaning all the results
        CleanResults ->
            ( { username = "", user = Waiting, repos = Waiting }, Cmd.none )



-- SUBSCRIPTION


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ div [ class "hero is-primary" ]
            [ div [ class "hero-body" ]
                [ p [ class "title" ]
                    [ text "Github User" ]
                , p [ class "subtitle" ]
                    [ text "Procure por usuários do github" ]
                ]
            ]
        , div [ class "container px-4" ]
            [ div [ class "box block mt-4" ]
                [ div [ class "field has-addons" ]
                    [ div [ class "control" ]
                        [ input [ class "input", placeholder "Digite um username", onInput UsernameChanged, value model.username ]
                            []
                        ]
                    , div [ class "control" ]
                        [ button [ class "button is-success", onClick UserRequested ] [ text "Pesquisar" ]
                        ]
                    , div [ class "control" ]
                        [ button [ class "button is-info", onClick CleanResults ] [ text "Limpar" ]
                        ]
                    ]
                ]
            , div [ class "columns" ]
                [ div [ class "column is-half" ] [ viewUser model.user ]
                , div [ class "column is-half" ] [ viewRepos model.repos ]
                ]
            ]
        ]


viewLoading : Html Msg
viewLoading =
    text "Carregando ..."


viewUser : Request User -> Html Msg
viewUser request =
    case request of
        Waiting ->
            text ""

        Loading ->
            viewLoading

        Failure error ->
            viewFailure error

        Success user ->
            div [ class "box block user-received" ]
                [ div [ class "columns" ]
                    [ div [ class "column is-half" ]
                        [ figure [ class "image is-square" ]
                            [ img [ class "is-rounded", src user.avatarUrl ] []
                            ]
                        ]
                    , div [ class "column is-half" ]
                        [ p [ class "is-size-4" ] [ strong [] [ text ("(" ++ user.login ++ ") " ++ user.name) ] ]
                        , p [] [ strong [] [ text "Empresa: " ], text user.company ]
                        , p [] [ strong [] [ text "Localização: " ], text user.location ]
                        , p [] [ strong [] [ text "Website: " ], a [ href user.blog ] [ text user.blog ] ]
                        , p [] [ strong [] [ text "Bio: " ], text user.bio ]
                        ]
                    ]
                ]


viewRepos : Request (List Repo) -> Html Msg
viewRepos request =
    case request of
        Waiting ->
            text ""

        Loading ->
            viewLoading

        Failure error ->
            viewFailure error

        Success repos ->
            div []
                [ h3 [ class "is-size-3 has-text-weight-bold mb-4" ] [ text "Repositórios" ]
                , ul
                    [ class "repos-received" ]
                    (List.map
                        (\item ->
                            li [ class "box block" ]
                                [ strong [ ] [ text item.fullName ]
                                , p [] [ text ("Descrição: " ++ item.description) ]
                                , p [] [ text ("Stars: " ++ String.fromInt item.stars) ]
                                , p [] [ text ("Watchers: " ++ String.fromInt item.watchers) ]
                                , p [] [ text ("Forks: " ++ String.fromInt item.forks) ]
                                , p [] [ text ("Linguagem: " ++ item.language) ]
                                ]
                        )
                        repos
                    )
                ]


viewFailure : Http.Error -> Html Msg
viewFailure error =
    case error of
        Http.BadUrl msg ->
            text ("URL inválido! " ++ msg)

        Http.Timeout ->
            text "O servidor demorou demais para retornar a resposta ..."

        Http.NetworkError ->
            text "Parece que sua internet tá meio zoada hein?"

        Http.BadStatus code ->
            text ("Erro do servidor. Código do erro: " ++ String.fromInt code)

        Http.BadBody msg ->
            text ("Erro ao converter seu JSON: " ++ msg)



-- HTTTP


getGithubUser : String -> Cmd Msg
getGithubUser username =
    Http.get
        { url = "https://api.github.com/users/" ++ username
        , expect = Http.expectJson UserReceived userDecoder
        }


getUserRepos : String -> Cmd Msg
getUserRepos username =
    Http.get
        { url = "https://api.github.com/users/" ++ username ++ "/repos?per_page=100"
        , expect = Http.expectJson ReposReceived reposDecoder
        }


userDecoder : Decoder User
userDecoder =
    Decode.succeed User
        |> required "login" string
        |> optional "name" string ""
        |> required "avatar_url" string
        |> optional "company" string ""
        |> optional "blog" string ""
        |> optional "location" string ""
        |> optional "bio" string ""


reposDecoder : Decoder (List Repo)
reposDecoder =
    Decode.list repoDecoder


repoDecoder : Decoder Repo
repoDecoder =
    Decode.succeed Repo
        |> required "name" string
        |> required "full_name" string
        |> required "html_url" string
        |> optional "description" string ""
        |> required "stargazers_count" int
        |> required "watchers_count" int
        |> required "forks_count" int
        |> optional "language" string ""



-- MAIN


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }
