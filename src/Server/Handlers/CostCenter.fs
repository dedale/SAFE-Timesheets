module Handlers.CostCenter

open Shared

open FSharp.Control.Tasks.ContextInsensitive
open Giraffe
open Microsoft.AspNetCore.Http

let addCostCenter (next: HttpFunc) (ctx: HttpContext) = task {
    // TODO db
    let! name = ctx.BindJsonAsync<string>()
    let id = CostCenterId (name.GetHashCode())
    let costCenter =
        { CostCenter.Id = id
          Name = name }
    return! ctx.WriteJsonAsync costCenter
}

let delCostCenter (id: int) (next: HttpFunc) (ctx: HttpContext) = task {
    // TODO del from db
    // TODO fail if used
    return! ctx.WriteJsonAsync ""
}
