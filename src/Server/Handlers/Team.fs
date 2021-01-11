module Handlers.Team

open Shared

open Database

open FSharp.Control.Tasks.ContextInsensitive
open FSharp.Data.Dapper
open Giraffe
open Microsoft.AspNetCore.Http
open Saturn.ControllerHelpers

let getTeams (next: HttpFunc) (ctx: HttpContext) = task {
    use connection = new FileConnection(defaultFile)
    let connectionF () = Connection.SqliteConnection connection.Value
    let team = Queries.Team connectionF
    let teams = team.GetAll() |> Async.map List.ofSeq |> Async.RunSynchronously
    return! ctx.WriteJsonAsync teams
}

let addTeam (next: HttpFunc) (ctx: HttpContext) = task {
    let! (teamName, managerId) = ctx.BindJsonAsync<string * UserId>()
    use connection = new FileConnection(defaultFile)
    let connectionF () = Connection.SqliteConnection connection.Value
    let team = Queries.Team connectionF
    let! created = team.New teamName managerId
    match created with
    | Some id ->
        let team =
            { Team.Id = id
              Name = teamName }
        return! ctx.WriteJsonAsync team
    | _ ->
        return! Response.internalError ctx ""
}

let delTeam (id: int) (next: HttpFunc) (ctx: HttpContext) = task {
    use connection = new FileConnection(defaultFile)
    let connectionF () = Connection.SqliteConnection connection.Value
    let team = Queries.Team connectionF
    let! _ = TeamId id |> team.Delete
    // TODO fail if team has users
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
    let! (name, costCenterId) = ctx.BindJsonAsync<string * CostCenterId>()
    use connection = new FileConnection(defaultFile)
    let connectionF () = Connection.SqliteConnection connection.Value
    let task = Queries.Task connectionF
    let! created = task.NewTeamTask (TeamId id) name costCenterId
    match created with
    | Some id ->
        let task =
            { Task.Id = id
              Name = name
              CostCenterId = costCenterId }
        return! ctx.WriteJsonAsync task
    | _ ->
        return! Response.internalError ctx ""
}
