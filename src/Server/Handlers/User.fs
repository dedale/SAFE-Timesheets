module Handlers.User

open Shared

open Database

open FSharp.Control.Tasks.ContextInsensitive
open FSharp.Data.Dapper
open Giraffe
open Microsoft.AspNetCore.Http
open Saturn.ControllerHelpers

open System.Text.RegularExpressions

let getUsers (next: HttpFunc) (ctx: HttpContext) = task {
    use connection = new FileConnection(defaultFile)
    let connectionF () = Connection.SqliteConnection connection.Value
    let user = Queries.User connectionF
    let users = user.GetAll() |> Async.map List.ofSeq |> Async.RunSynchronously
    return! ctx.WriteJsonAsync users
}

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

let fullName (login: UserLogin) =
    match login.Value with
    | "manager1" -> Ok "Mike ANAGER"
    | value ->
        let m = Regex.Match(value, "^[a-z]+$")
        if m.Success then
            let first = dummyNames |> List.find (fun x -> x.[0] = value.ToUpperInvariant().[0])
            let last = value.Substring(1).ToUpperInvariant()
            sprintf "%s %s" first last |> Ok
        else
            Error (sprintf "Unknown user '%s'" value)

let validate (username: string) = task {
    // TODO get name from Active Directory?
    match UserLogin.create (username.ToLowerInvariant()) with
    | Ok login ->
        match fullName login with
        | Ok full ->
            use connection = new FileConnection(defaultFile)
            let connectionF () = Connection.SqliteConnection connection.Value
            let user = Queries.User connectionF
            let! created = user.New login full None None
            match created with
            | Some id ->
                return Ok
                    { User.Id = id
                      Name = full
                      Login = login }
            | None -> return Error "Failed to create user"
        | Error m -> return Error (sprintf "Unknown user '%s'" login.Value)
    | Error m -> return Error m
}

let addUser (next: HttpFunc) (ctx: HttpContext) = task {
    let! username = ctx.BindJsonAsync<string>()
    let! validated = validate username
    return!
        match validated with
        | Ok user ->
            ctx.WriteJsonAsync user
        | Error m ->
            Response.badRequest ctx (sprintf "Invalid user '%s': %s." username m)
}

let delUser (id: int) (next: HttpFunc) (ctx: HttpContext) = task {
    use connection = new FileConnection(defaultFile)
    let connectionF () = Connection.SqliteConnection connection.Value
    let user = Queries.User connectionF
    let! _ = UserId id |> user.Delete
    // TODO fail if used (handled by constraints?)
    return! ctx.WriteJsonAsync ""
}
