module Pages.Home

open Client.Domain

open Elmish
open Feliz
open Feliz.Router

type State =
    { User : ApplicationUser }

type Msg = Msg of unit

let init user =
    { User = user }, Cmd.none

let render (state: State) (dispatch: Msg -> unit) =
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
