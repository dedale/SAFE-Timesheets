module App

open Elmish
open Feliz
open Feliz.Router
//open Thoth.Fetch

open Shared

[<RequireQualifiedAccess>]
type Page =
    | Login of Pages.Login.State
    | Admin of Pages.Admin.State
    | Index
    | NotFound

[<RequireQualifiedAccessAttribute>]
type Url =
    | Index
    | NotFound
    | Login
    | Admin
    | Logout

let parseUrl = function
    | [  ] -> Url.Index
    | [ "login" ] -> Url.Login
    | [ "admin" ] -> Url.Admin
    | [ "logout" ] -> Url.Logout
    | _ -> Url.NotFound

type ApplicationUser =
    | Anonymous
    | LoggedIn of LoggedUser

type State =
    { CurrentPage : Page
      CurrentUrl  : Url
      User : ApplicationUser }

type Msg =
    | LoginMsg of Pages.Login.Msg
    | AdminMsg of Pages.Admin.Msg
    | UrlChanged of Url

let init() =
    let initialUrl = parseUrl (Router.currentUrl())
    let defaultState =
        { User = Anonymous
          CurrentUrl = initialUrl
          CurrentPage = Page.Index }

    match initialUrl with
    | Url.Index ->
        defaultState, Cmd.none

    | Url.Login ->
        let loginState, loginCmd = Pages.Login.init()
        let nextPage = Page.Login loginState
        { defaultState with CurrentPage = nextPage }, Cmd.map LoginMsg loginCmd

    | Url.Admin ->
        defaultState, Router.navigate("admin", HistoryMode.ReplaceState)

    | Url.Logout ->
        defaultState, Router.navigate("/", HistoryMode.ReplaceState)

    | Url.NotFound ->
        { defaultState with CurrentPage = Page.NotFound }, Cmd.none

let update (msg: Msg) (state: State) : State * Cmd<Msg> =
    match msg, state.CurrentPage with
    //| GotHello hello ->
    //    { model with Hello = hello }, Cmd.none
    
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

    | UrlChanged nextUrl, _ ->
        let show page = { state with CurrentPage = page; CurrentUrl = nextUrl }

        match nextUrl with
        | Url.Index -> show Page.Index, Cmd.none
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

        | Url.Logout ->
            { state with User = Anonymous }, Router.navigate("/")

    | _, _ ->
        state, Cmd.none

let index (state: State) (dispatch: Msg -> unit) =
    match state.User with
    | Anonymous ->
        Html.div [
            Html.h1 "Welcome, guest"
            Html.a [
                prop.className [ "button"; "is-info" ]
                prop.style [ style.margin 5 ]
                prop.href (Router.format("login"))
                prop.text "Login"
            ]
        ]

    | LoggedIn user ->
        Html.div [
            Html.h1 (sprintf "Welcome, %s" user.Username)
            Html.a [
                prop.className [ "button"; "is-info" ]
                prop.style [ style.margin 5 ]
                prop.href (Router.format("admin"))
                prop.text "Admin"
            ]
            Html.a [
                prop.className [ "button"; "is-info" ]
                prop.style [ style.margin 5 ]
                prop.href (Router.format("logout"))
                prop.text "Logout"
            ]
        ]

let render (state: State) (dispatch: Msg -> unit) =
    let activePage =
        match state.CurrentPage with
        | Page.Login login -> Pages.Login.render login (LoginMsg >> dispatch)
        | Page.Admin admin -> Pages.Admin.render admin (AdminMsg >> dispatch)
        | Page.Index -> index state dispatch
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
