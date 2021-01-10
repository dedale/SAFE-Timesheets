module Handlers.Team

open Shared

open Database

open FSharp.Control.Tasks.ContextInsensitive
open FSharp.Data.Dapper
open Giraffe
open Microsoft.AspNetCore.Http

let getTeams (next: HttpFunc) (ctx: HttpContext) = task {
    use connection = new FileConnection(defaultFile)
    let connectionF () = Connection.SqliteConnection connection.Value
    let team = Queries.Team connectionF
    let teams = team.GetAll() |> Async.map List.ofSeq |> Async.RunSynchronously
    return! ctx.WriteJsonAsync teams
}

let addTeam (next: HttpFunc) (ctx: HttpContext) = task {
    // TODO db
    let! (teamName, managerId) = ctx.BindJsonAsync<string * UserId>()
    let id = TeamId (teamName.GetHashCode())
    let team =
        { Team.Id = id
          Name = teamName }
    return! ctx.WriteJsonAsync team
}

let delTeam (id: int) (next: HttpFunc) (ctx: HttpContext) = task {
    // TODO del from db
    // TODO fail if not empty
    return! ctx.WriteJsonAsync ""
}
