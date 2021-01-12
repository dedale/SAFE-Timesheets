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
    | Some teamId ->
        let team =
            { Team.Id = teamId
              Name = teamName }
        return! ctx.WriteJsonAsync team
    | _ ->
        return! Response.internalError ctx ""
}

let delTeam (teamId: int) (next: HttpFunc) (ctx: HttpContext) = task {
    use connection = new FileConnection(defaultFile)
    let connectionF () = Connection.SqliteConnection connection.Value
    let team = Queries.Team connectionF
    let! _ = TeamId teamId |> team.Delete
    // TODO fail if team has users
    return! ctx.WriteJsonAsync ""
}

let getTeamUsers (teamId: int) (next: HttpFunc) (ctx: HttpContext) = task {
    use connection = new FileConnection(defaultFile)
    let connectionF () = Connection.SqliteConnection connection.Value
    let user = Queries.User connectionF
    let users = TeamId teamId |> user.GetByTeam |> Async.map List.ofSeq |> Async.RunSynchronously
    return! ctx.WriteJsonAsync users
}

let getTeamTasks (teamId: int) (next: HttpFunc) (ctx: HttpContext) = task {
    use connection = new FileConnection(defaultFile)
    let connectionF () = Connection.SqliteConnection connection.Value
    let task = Queries.Task connectionF
    let tasks = TeamId teamId |> task.GetByTeam |> Async.map List.ofSeq |> Async.RunSynchronously
    return! ctx.WriteJsonAsync tasks
}

let addTeamTask (teamId: int) (next: HttpFunc) (ctx: HttpContext) = task {
    let! (name, costCenterId) = ctx.BindJsonAsync<string * CostCenterId>()
    use connection = new FileConnection(defaultFile)
    let connectionF () = Connection.SqliteConnection connection.Value
    let task = Queries.Task connectionF
    let! created = task.NewTeamTask (TeamId teamId) name costCenterId
    match created with
    | Some taskId ->
        let task =
            { Task.Id = taskId
              Name = name
              CostCenterId = costCenterId }
        return! ctx.WriteJsonAsync task
    | _ ->
        return! Response.internalError ctx ""
}
