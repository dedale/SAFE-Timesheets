module Handlers.Task

open Shared

open Database

open FSharp.Data.Dapper
open FSharp.Control.Tasks.ContextInsensitive
open Giraffe
open Microsoft.AspNetCore.Http
open Saturn.ControllerHelpers

let getTasks (next: HttpFunc) (ctx: HttpContext) = task {
    use connection = new FileConnection(defaultFile)
    let connectionF () = Connection.SqliteConnection connection.Value
    let task = Queries.Task connectionF
    let tasks = task.GetCommon() |> Async.map List.ofSeq |> Async.RunSynchronously
    return! ctx.WriteJsonAsync tasks
}

let addTask (next: HttpFunc) (ctx: HttpContext) = task {
    let! (name, costCenterId) = ctx.BindJsonAsync<string * CostCenterId>()
    use connection = new FileConnection(defaultFile)
    let connectionF () = Connection.SqliteConnection connection.Value
    let task = Queries.Task connectionF
    let! created = task.New name costCenterId
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

let delTask (taskId: int) (next: HttpFunc) (ctx: HttpContext) = task {
    use connection = new FileConnection(defaultFile)
    let connectionF () = Connection.SqliteConnection connection.Value
    let task = Queries.Task connectionF
    let! _ = TaskId taskId |> task.Delete
    // TODO fail if used (handled by constraints?)
    return! ctx.WriteJsonAsync ""
}
