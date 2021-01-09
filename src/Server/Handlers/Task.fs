module Handlers.Task

open Shared

open FSharp.Control.Tasks.ContextInsensitive
open Giraffe
open Microsoft.AspNetCore.Http

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
