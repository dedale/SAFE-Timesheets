module Client.Domain

open Shared

type ApplicationUser =
    | Anonymous
    | LoggedIn of LoggedUser

[<RequireQualifiedAccessAttribute>]
type Url =
    | Home
    | NotFound
    | Login
    | Admin
    | Team
    | Logout
