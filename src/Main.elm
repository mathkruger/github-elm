module Main exposing (..)

import Browser
import Consts exposing (..)
import Html exposing (Html, a, button, div, figure, h3, img, input, li, p, strong, text, ul)
import Html.Attributes exposing (class, href, placeholder, src, target, value)
import Html.Events exposing (onClick, onInput)
import Http
import Json.Decode as Decode exposing (Decoder, int, string)
import Json.Decode.Pipeline exposing (optional, required)



-- MODEL


type alias Model =
    { username : String
    , repoNameFilter : String
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


getCleanModel : () -> Model
getCleanModel _ =
    { username = "", repoNameFilter = "", user = Waiting, repos = Waiting }


init : () -> ( Model, Cmd Msg )
init _ =
    ( getCleanModel (), Cmd.none )



-- UPDATE


type Msg
    = UsernameChanged String
    | RepoNameFilterChanged String
    | UserRequested
    | UserReceived (Result Http.Error User)
    | ReposReceived (Result Http.Error (List Repo))
    | CleanResults


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UsernameChanged newUsername ->
            ( { model | username = newUsername }, Cmd.none )

        RepoNameFilterChanged newReponame ->
            ( { model | repoNameFilter = newReponame }, Cmd.none )

        UserRequested ->
            ( { model | user = Loading }, getGithubUser model.username )

        UserReceived result ->
            case result of
                Ok user ->
                    ( { model | user = Success user, repos = Loading }, getUserRepos model.username )

                Err err ->
                    ( { model | user = Failure err }, Cmd.none )

        ReposReceived result ->
            case result of
                Ok repos ->
                    ( { model | repos = Success repos }, Cmd.none )

                Err err ->
                    ( { model | repos = Failure err }, Cmd.none )

        -- Cleaning all the results
        CleanResults ->
            ( getCleanModel (), Cmd.none )



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
            , div [ class "columns is-multiline is-mobile" ]
                [ div [ class "column is-full mb-4" ] [ viewUser model.user ]
                , div [ class "column is-full" ] [ viewRepos model.repos model ]
                ]
            ]
        ]


viewLoading : Html Msg
viewLoading =
    div [ class "loader" ] []


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
                [ div [ class "columns is-vcentered" ]
                    [ div [ class "column is-narrow" ]
                        [ figure [ class "image is-128x128" ]
                            [ img [ class "is-rounded", src user.avatarUrl ] []
                            ]
                        ]
                    , div [ class "column" ]
                        [ p [ class "is-size-4" ] [ strong [] [ text ("(" ++ user.login ++ ") " ++ user.name) ] ]
                        , p [] [ strong [] [ text "Empresa: " ], text user.company ]
                        , p [] [ strong [] [ text "Localização: " ], text user.location ]
                        , p [] [ strong [] [ text "Website: " ], a [ href user.blog, target "_blank" ] [ text user.blog ] ]
                        , p [] [ strong [] [ text "Bio: " ], text user.bio ]
                        ]
                    ]
                ]


viewRepos : Request (List Repo) -> Model -> Html Msg
viewRepos request model =
    case request of
        Waiting ->
            text ""

        Loading ->
            viewLoading

        Failure error ->
            viewFailure error

        Success repos ->
            div [ ]
                [ h3 [ class "is-size-3 has-text-weight-bold mb-4" ] [ text "Repositórios" ]
                , input [ class "input mb-2", placeholder "Filtrar repositórios", onInput RepoNameFilterChanged, value model.repoNameFilter ]
                    []
                , ul
                    [ class "columns is-multiline is-mobile repos-received" ]
                    (repos
                        |> List.filter (\item -> filterRepos model.repoNameFilter item.name)
                        |> List.map (\item -> viewSingleRepo item)
                    )
                ]


viewSingleRepo : Repo -> Html msg
viewSingleRepo item =
    li [ class "column is-one-third" ]
        [ a [ href (githubUrlPrefix ++ item.fullName), class "box block", target "_blank" ]
            [ strong [] [ text item.name ]
            , p [ class "text-limited" ] [ text ("Descrição: " ++ item.description) ]
            , p [] [ text ("Stars: " ++ String.fromInt item.stars) ]
            , p [] [ text ("Watchers: " ++ String.fromInt item.watchers) ]
            , p [] [ text ("Forks: " ++ String.fromInt item.forks) ]
            , p [] [ text ("Linguagem: " ++ item.language) ]
            ]
        ]


filterRepos : String -> String -> Bool
filterRepos filterTerm field =
    String.isEmpty filterTerm || String.contains (String.toLower filterTerm) (String.toLower field)


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
        { url = githubApiPrefix ++ username
        , expect = Http.expectJson UserReceived userDecoder
        }


getUserRepos : String -> Cmd Msg
getUserRepos username =
    Http.get
        { url = githubApiPrefix ++ username ++ "/repos?per_page=100"
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
