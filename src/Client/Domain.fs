module Client.Domain

open Shared

type ApplicationUser =
    | Anonymous
    | LoggedIn of LoggedUser
