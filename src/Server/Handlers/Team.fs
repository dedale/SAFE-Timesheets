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

let getTeamUsers (id: int) (next: HttpFunc) (ctx: HttpContext) = task {
    use connection = new FileConnection(defaultFile)
    let connectionF () = Connection.SqliteConnection connection.Value
    let user = Queries.User connectionF
    let users = TeamId id |> user.GetByTeam |> Async.map List.ofSeq |> Async.RunSynchronously
    return! ctx.WriteJsonAsync users
}

let getTeamTasks (id: int) (next: HttpFunc) (ctx: HttpContext) = task {
    use connection = new FileConnection(defaultFile)
    let connectionF () = Connection.SqliteConnection connection.Value
    let task = Queries.Task connectionF
    let tasks = TeamId id |> task.GetByTeam |> Async.map List.ofSeq |> Async.RunSynchronously
    return! ctx.WriteJsonAsync tasks
}

let addTeamTask (id: int) (next: HttpFunc) (ctx: HttpContext) = task {
    // TODO db
    let! (name, costCenterId) = ctx.BindJsonAsync<string * CostCenterId>()
    let id = TaskId (name.GetHashCode())
    let task =
        { Task.Id = id
          Name = name
          CostCenterId = costCenterId }
    return! ctx.WriteJsonAsync task
}
