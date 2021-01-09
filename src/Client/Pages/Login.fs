module Pages.Login

// Adapted from https://github.com/Zaid-Ajaj/login-with-url-extended
// and from https://github.com/SAFE-Stack/SAFE-BookStore

open Shared

open Elmish
open Fable.Core.JsInterop
open Fable.FontAwesome
open Feliz
open Feliz.Bulma
open Fetch.Types
open System
#if FABLE_COMPILER
open Thoth.Json
#else
open Thoth.Json.Net
#endif

type LoginResult =
    | LoggedIn of LoggedUser
    | AuthError of string // exn : [ELMISH DEBUGGER] Cannot generate auto encoder for System.Exception. Please pass an extra encoder.

type State =
    { Credentials : UserCredentials
      LoginAttempt : Deferred<LoginResult> }

type Msg =
    | UsernameChanged of string
    | PasswordChanged of string
    | LogInClicked of PromiseStatus<LoginResult>

let (|UserLoggedIn|_|) = function
    | LogInClicked (Completed (LoginResult.LoggedIn user)) -> Some user
    | _ -> None

let authUser (credentials: UserCredentials) = promise {
    if String.IsNullOrEmpty credentials.Username then
        return! failwithf "You need to fill in a username."
    elif String.IsNullOrEmpty credentials.Password then
        return! failwithf "You need to fill in a password."
    else
        let body = Encode.Auto.toString(0, credentials)
        let props = [
            Method HttpMethod.POST
            Fetch.requestHeaders [ ContentType "application/json" ]
            Body !^body
        ]
        let! res = Fetch.fetch Route.login props
        let! txt = res.text()
        return Decode.Auto.unsafeFromString<LoggedUser> txt |> (LoggedIn >> Completed)
}

let init () =
    { Credentials = { Username = ""; Password = ""; PasswordId = Guid.NewGuid() }
      LoginAttempt = NotStarted }, Cmd.none

let update (msg: Msg) (state: State) =
    match msg with
    | UsernameChanged username ->
        { state with Credentials = { state.Credentials with Username = username } }, Cmd.none

    | PasswordChanged password ->
        { state with Credentials = { state.Credentials with Password = password } }, Cmd.none

    | LogInClicked Pending ->
        let nextState = { state with LoginAttempt = InProgress }
        let OnFailed (e: exn) =
            AuthError e.Message |> (Completed >> LogInClicked)
        let nextCmd = Cmd.OfPromise.either authUser state.Credentials LogInClicked OnFailed
        nextState, nextCmd

    | LogInClicked (Completed loginResult) ->
        let nextState = { state with LoginAttempt = Resolved loginResult }
        nextState, Cmd.none

let renderLoginOutcome (loginResult: Deferred<LoginResult>)=
    match loginResult with
    | Resolved (LoginResult.AuthError e) ->
        // TODO if debug
        printfn "%A" e
        Bulma.help [
            color.isDanger
            prop.text "Authentication failure."
        ]

    | Resolved (LoginResult.LoggedIn user) ->
        Bulma.help [
            color.isDanger
            prop.text (sprintf "User '%s' has succesfully logged in" (UserLogin.value user.Username))
        ]

    | _ ->
        Html.none

let layout (children: ReactElement list) =
    Html.section [
        Bulma.hero [
            hero.isFullHeight
            prop.children [
                Bulma.container [
                    Bulma.columns [
                        columns.isCentered
                        prop.children [
                            Bulma.column [
                                //column.isHalfTablet; column.isOneThirdDesktop; column.isOneThirdWideScreen
                                prop.children children
                            ]
                        ]
                    ]
                ]
            ]
        ]
    ]

let centered (children: ReactElement list) =
    Html.div [
        prop.style [
            style.margin.auto
            style.textAlign.center
            style.width (length.percent 100)
        ]
        prop.children children
    ]

let render (state: State) (dispatch: Msg -> unit) =
    layout [
        Html.div [
            prop.className "box"
            prop.children [

                centered [
                    Html.img [
                        prop.src "https://fable.io/img/fable_logo.png"
                        prop.height 160
                        prop.width 140
                    ]
                ]

                Bulma.field.div [
                    Bulma.label "Username"
                    Bulma.control.div [
                        control.hasIconsLeft
                        prop.children [
                            Bulma.input.text [
                                prop.placeholder "Username"
                                prop.valueOrDefault state.Credentials.Username
                                prop.onChange (UsernameChanged >> dispatch)
                            ]
                            Bulma.icon [
                                icon.isSmall; icon.isLeft
                                prop.children [
                                    Fa.i [ Fa.Solid.User ] []
                                ]
                            ]
                        ]
                    ]
                ]
                Bulma.field.div [
                    Bulma.label "Password"
                    Bulma.control.div [
                        control.hasIconsLeft
                        prop.children [
                            Bulma.input.password [
                                prop.placeholder "********"
                                prop.valueOrDefault state.Credentials.Password
                                prop.onChange (PasswordChanged >> dispatch)
                            ]
                            Bulma.icon [
                                icon.isSmall; icon.isLeft
                                prop.children [
                                    Fa.i [ Fa.Solid.Lock ] []
                                ]
                            ]
                        ]
                    ]
                ]
                Bulma.field.div [
                    Bulma.button.button [
                        color.isInfo
                        prop.className "is-fullwidth"
                        if state.LoginAttempt = InProgress then button.isLoading
                        prop.onClick (fun _ -> dispatch (LogInClicked Pending))
                        prop.text "Login"
                    ]
                ]
                renderLoginOutcome state.LoginAttempt
            ]
        ]
    ]

