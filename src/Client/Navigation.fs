module Client.Navigation

open Domain

open Fable.FontAwesome
open Feliz
open Feliz.Bulma
open Feliz.Router

let renderNavigation (user: ApplicationUser) (url: Url) =
    Bulma.navbar [
        prop.children [
            Bulma.navbarBrand.a [
                prop.href "https://safe-stack.github.io/"
                spacing.pr3
                prop.children [
                    Html.img [
                        prop.style [
                            style.width 45
                            style.height 26
                            style.marginTop length.auto
                            style.marginBottom length.auto
                        ]
                        prop.src "/safe_logo.png"
                        prop.alt "SAFE-Stack"
                    ]
                ]
            ]
            Bulma.navbarMenu [
                Bulma.navbarStart.div [
                    match user with
                    | LoggedIn loggedin ->
                        if url = Url.Admin || url = Url.Team then
                            Bulma.navbarItem.a [
                                color.isInfo
                                spacing.px3
                                prop.href (Router.format(""))
                                prop.children [
                                    Bulma.icon [
                                        Fa.i [ Fa.Solid.Home ] []
                                    ]
                                    Html.span "Home"
                                ]
                            ]
                        if url = Url.Home && not loggedin.ManagedTeams.IsEmpty then
                            Bulma.navbarItem.a [
                                color.isInfo
                                spacing.px3
                                prop.href (Router.format("team"))
                                prop.children [
                                    Bulma.icon [
                                        Fa.i [ Fa.Solid.Users ] []
                                    ]
                                    Html.span "Team"
                                ]
                            ]
                        if url = Url.Home && loggedin.IsAdmin then
                            Bulma.navbarItem.a [
                                color.isInfo
                                spacing.px3
                                prop.href (Router.format("admin"))
                                prop.children [
                                    Bulma.icon [
                                        Fa.i [ Fa.Solid.UserShield ] []
                                    ]
                                    Html.span "Admin"
                                ]
                            ]
                    | _ -> Html.none
                ]
                Bulma.navbarEnd.div [
                    Bulma.buttons [
                        match user with
                        | LoggedIn _ ->
                            Bulma.navbarItem.a [
                                color.isInfo
                                prop.href (Router.format("logout"))
                                prop.children [
                                    Bulma.icon [
                                        Fa.i [ Fa.Solid.User ] []
                                    ]
                                    Html.span "Logout"
                                ]
                            ]
                        | Anonymous ->
                            Bulma.navbarItem.a [
                                color.isInfo
                                prop.href (Router.format("login"))
                                prop.children [
                                    Bulma.icon [
                                        Fa.i [ Fa.Solid.User ] []
                                    ]
                                    Html.span "Login"
                                ]
                            ]
                    ]
                ]
                Bulma.navbarDivider []
            ]
        ]
    ]

