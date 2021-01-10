module Handlers.Task

open Shared

open Database

open FSharp.Data.Dapper
open FSharp.Control.Tasks.ContextInsensitive
open Giraffe
open Microsoft.AspNetCore.Http

let getTasks (next: HttpFunc) (ctx: HttpContext) = task {
    use connection = new FileConnection(defaultFile)
    let connectionF () = Connection.SqliteConnection connection.Value
    let task = Queries.Task connectionF
    let tasks = task.GetAll() |> Async.map List.ofSeq |> Async.RunSynchronously
    return! ctx.WriteJsonAsync tasks
}

let addTask (next: HttpFunc) (ctx: HttpContext) = task {
    // TODO db
    let! (name, costCenterId) = ctx.BindJsonAsync<string * CostCenterId>()
    let id = TaskId (name.GetHashCode())
    let task =
        { Task.Id = id
          Name = name
          CostCenterId = costCenterId }
    return! ctx.WriteJsonAsync task
}

let delTask (id: int) (next: HttpFunc) (ctx: HttpContext) = task {
    // TODO del from db
    // TODO fail if not empty
    return! ctx.WriteJsonAsync ""
}
