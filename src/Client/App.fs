module App

open Elmish
open Feliz
open Feliz.Router

open Shared
open Client.Domain

[<RequireQualifiedAccess>]
type Page =
    | Login of Pages.Login.State
    | Admin of Pages.Admin.State
    | Home of Pages.Home.State
    | Team of Pages.Team.State
    | NotFound

[<RequireQualifiedAccessAttribute>]
type Url =
    | Home
    | NotFound
    | Login
    | Admin
    | Team
    | Logout

let parseUrl = function
    | [  ] -> Url.Home
    | [ "login" ] -> Url.Login
    | [ "admin" ] -> Url.Admin
    | [ "team" ] -> Url.Team
    | [ "logout" ] -> Url.Logout
    | _ -> Url.NotFound

type State =
    { CurrentPage : Page
      CurrentUrl  : Url
      User : ApplicationUser }

type Msg =
    | HomeMsg of Pages.Home.Msg
    | LoginMsg of Pages.Login.Msg
    | AdminMsg of Pages.Admin.Msg
    | TeamMsg of Pages.Team.Msg
    | UrlChanged of Url

let init() =
    let initialUrl = parseUrl (Router.currentUrl())
    let defaultState =
        { User = Anonymous
          CurrentUrl = initialUrl
          CurrentPage = Page.NotFound }

    match initialUrl with
    | Url.Home ->
        let homeState, homeCmd = Pages.Home.init defaultState.User
        let nextPage = Page.Home homeState
        { defaultState with CurrentPage = nextPage }, Cmd.map HomeMsg homeCmd

    | Url.Login ->
        let loginState, loginCmd = Pages.Login.init()
        let nextPage = Page.Login loginState
        { defaultState with CurrentPage = nextPage }, Cmd.map LoginMsg loginCmd

    | Url.Admin ->
        defaultState, Router.navigate("admin", HistoryMode.ReplaceState)

    | Url.Team ->
        defaultState, Router.navigate("team", HistoryMode.ReplaceState)

    | Url.Logout ->
        defaultState, Router.navigate("/", HistoryMode.ReplaceState)

    | Url.NotFound ->
        { defaultState with CurrentPage = Page.NotFound }, Cmd.none

let update (msg: Msg) (state: State) : State * Cmd<Msg> =
    match msg, state.CurrentPage with
    
    | LoginMsg loginMsg, Page.Login loginState ->
        match loginMsg with
        | Pages.Login.UserLoggedIn user ->
            { state with User = LoggedIn user }, Router.navigate("/")

        | loginMsg ->
            let loginState, loginCmd = Pages.Login.update loginMsg loginState
            { state with CurrentPage = Page.Login loginState }, Cmd.map LoginMsg loginCmd

    | AdminMsg adminMsg, Page.Admin adminState ->
        let adminState, adminCmd = Pages.Admin.update adminMsg adminState
        { state with CurrentPage = Page.Admin adminState }, Cmd.map AdminMsg adminCmd

    | TeamMsg teamMsg, Page.Team teamState ->
        let teamState, teamCmd = Pages.Team.update teamMsg teamState
        { state with CurrentPage = Page.Team teamState }, Cmd.map TeamMsg teamCmd

    | UrlChanged nextUrl, _ ->
        let show page = { state with CurrentPage = page; CurrentUrl = nextUrl }

        match nextUrl with
        | Url.Home ->
            let homeState, homeCmd = Pages.Home.init state.User
            show (Page.Home homeState), Cmd.map HomeMsg homeCmd

        | Url.NotFound -> show Page.NotFound, Cmd.none

        | Url.Login ->
            let loginState, loginCmd = Pages.Login.init()
            show (Page.Login loginState), Cmd.map LoginMsg loginCmd

        | Url.Admin ->
            match state.User with
            | Anonymous -> state, Router.navigate("login", HistoryMode.ReplaceState)
            | LoggedIn user ->
                let adminState, adminCmd = Pages.Admin.init user
                show (Page.Admin adminState), Cmd.map AdminMsg adminCmd

        | Url.Team ->
            match state.User with
            | Anonymous -> state, Router.navigate("login", HistoryMode.ReplaceState)
            | LoggedIn user ->
                if user.IsManager then
                    let teamState, teamCmd = Pages.Team.init user
                    show (Page.Team teamState), Cmd.map TeamMsg teamCmd
                else
                    state, Router.navigate("login", HistoryMode.ReplaceState)

        | Url.Logout ->
            { state with User = Anonymous }, Router.navigate("/")

    | _, _ ->
        state, Cmd.none

let render (state: State) (dispatch: Msg -> unit) =
    let activePage =
        match state.CurrentPage with
        | Page.Login login -> Pages.Login.render login (LoginMsg >> dispatch)
        | Page.Admin admin -> Pages.Admin.render admin (AdminMsg >> dispatch)
        | Page.Home home -> Pages.Home.render home (HomeMsg >> dispatch)
        | Page.Team team -> Pages.Team.render team (TeamMsg >> dispatch)
        | Page.NotFound -> Html.h1 "Not Found"

    Router.router [
        Router.onUrlChanged (parseUrl >> UrlChanged >> dispatch)
        Router.application [
            Html.div [
                prop.style [ style.padding 20 ]
                prop.children [ activePage ]
            ]
        ]
    ]
