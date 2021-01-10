module Handlers.CostCenter

open Shared

open Database

open FSharp.Data.Dapper
open FSharp.Control.Tasks.ContextInsensitive
open Giraffe
open Microsoft.AspNetCore.Http

let getCostCenters (next: HttpFunc) (ctx: HttpContext) = task {
    use connection = new FileConnection(defaultFile)
    let connectionF () = Connection.SqliteConnection connection.Value
    let costCenter = Queries.CostCenter connectionF
    let costCenters = costCenter.GetAll() |> Async.map List.ofSeq |> Async.RunSynchronously
    return! ctx.WriteJsonAsync costCenters
}

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
