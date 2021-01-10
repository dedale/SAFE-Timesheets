module Pages.Home

open Client.Domain

open Shared

open Elmish
open Feliz
open Feliz.Bulma
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
            Bulma.button.a [
                color.isInfo
                prop.style [ style.margin 5 ]
                prop.href (Router.format("login"))
                prop.text "Login"
            ]
        ]

    | LoggedIn user ->
        Html.div [
            Html.h1 (sprintf "Welcome, %s" user.Username.Value)
            if user.IsAdmin then
                Bulma.button.a [
                    color.isInfo
                    prop.style [ style.margin 5 ]
                    prop.href (Router.format("admin"))
                    prop.text "Admin"
                ]
            if user.IsManager then
                Bulma.button.a [
                    color.isInfo
                    prop.style [ style.margin 5 ]
                    prop.href (Router.format("team"))
                    prop.text "Team"
                ]
            Bulma.button.a [
                color.isInfo
                prop.style [ style.margin 5 ]
                prop.href (Router.format("logout"))
                prop.text "Logout"
            ]
        ]
