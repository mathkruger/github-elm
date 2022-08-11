module Main exposing (..)

import Browser
import Html exposing (Html, a, button, div, h1, h2, img, input, li, p, text, ul)
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
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    div [ class "application-wrapper" ]
        [ div [ class "header" ] [ h1 [] [ text "Github User" ] ]
        , div [ class "input-form" ]
            [ input [ placeholder "Digite o usuário", onInput UsernameChanged, value model.username ] []
            , button [ class "submit-button", onClick UserRequested ] [ text "Pesquisar" ]
            , button [ class "clean-button", onClick CleanResults ] [ text "Limpar" ]
            ]
        , div [ class "user-received" ] [ viewUser model.user ]
        , h2 [] [ text "Repositórios" ]
        , div [ class "repos-received" ] [ viewRepos model.repos ]
        ]


viewUser : Request User -> Html Msg
viewUser request =
    case request of
        Waiting ->
            text "Digite um usuário para obter o perfil"

        Loading ->
            text "Carregando"

        Failure error ->
            viewFailure error

        Success user ->
            div []
                [ h2 [] [ text ("(" ++ user.login ++ ") " ++ user.name) ]
                , img [ src user.avatarUrl ] []
                , p [] [ text ("Empresa: " ++ user.company) ]
                , p [] [ text ("Localização: " ++ user.location) ]
                , p [] [ text "Website: ", a [ href user.blog ] [ text user.blog ] ]
                , p [] [ text ("Bio: " ++ user.bio) ]
                ]


viewRepos : Request (List Repo) -> Html Msg
viewRepos request =
    case request of
        Waiting ->
            text ""

        Loading ->
            text "Carregando repositórios ..."

        Failure error ->
            viewFailure error

        Success repos ->
            ul
                [ class "repo-list" ]
                (List.map
                    (\item ->
                        li []
                            [ p [] [ text item.fullName ]
                            , p [] [ text ("Descrição: " ++ item.description) ]
                            , p [] [ text ("Stars: " ++ (String.fromInt item.stars)) ]
                            , p [] [ text ("Watchers: " ++ (String.fromInt item.watchers)) ]
                            , p [] [ text ("Forks: " ++ (String.fromInt item.forks)) ]
                            , p [] [ text ("Linguagem: " ++ item.language) ]
                            ]
                    ) repos
                )


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


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }
