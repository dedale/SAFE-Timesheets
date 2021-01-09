module Handlers.User

open Shared

open FSharp.Control.Tasks.ContextInsensitive
open Giraffe
open Microsoft.AspNetCore.Http
open Saturn.ControllerHelpers

open System.Text.RegularExpressions

let dummyNames = [
    "Archibald"
    "Brittany"
    "Cage"
    "Dawn"
    "Edison"
    "Falynn"
    "Gene"
    "Haiden"
    "Indiana"
    "Jill"
    "Keaton"
    "Leane"
    "Marvin"
    "Niki"
    "Orson"
    "Polly"
    "Quirin"
    "Ripley"
    "Sewell"
    "Teri"
    "Usher"
    "Velvet"
    "Wadley"
    "Xenia"
    "Yule"
    "Zadie"
]

let validate (username: string) =
    // TODO get name from Active Directory?
    // TODO add in DB
    match UserLogin.create (username.ToLowerInvariant()) with
    | Ok login ->
        match UserLogin.value login with
        | "manager1" ->
            { Id = UserId 3
              User.Name = "Mike ANAGER"
              Login = login } |> Ok
        | value ->
            let m = Regex.Match(value, "^[a-z]+$")
            if m.Success then
                let first = dummyNames |> List.find (fun x -> x.[0] = value.ToUpperInvariant().[0])
                let last = value.Substring(1).ToUpperInvariant()
                { Id = UserId (value.GetHashCode())
                  User.Name = sprintf "%s %s" first last
                  Login = login } |> Ok
            else
                Error (sprintf "Unknown user '%s'" value)
    | Error m -> Error m

let addUser (next: HttpFunc) (ctx: HttpContext) = task {
    let! username = ctx.BindJsonAsync<string>()
    return!
        match validate username with
        | Ok user ->
            ctx.WriteJsonAsync user
        | Error m ->
            Response.badRequest ctx (sprintf "Invalid user '%s': %s." username m)
}

let delUser (id: int) (next: HttpFunc) (ctx: HttpContext) = task {
    // TODO del from db
    return! ctx.WriteJsonAsync ""
}
