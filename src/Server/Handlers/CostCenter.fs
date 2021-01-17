module Handlers.CostCenter

open Shared

open Database

open FSharp.Data.Dapper
open FSharp.Control.Tasks.ContextInsensitive
open Giraffe
open Microsoft.AspNetCore.Http
open Saturn.ControllerHelpers

let getCostCenters (next: HttpFunc) (ctx: HttpContext) = task {
    use connection = new FileConnection(defaultFile)
    let connectionF () = Connection.SqliteConnection connection.Value
    let queries = Queries.CostCenter connectionF
    let costCenters = queries.GetAll() |> Async.map List.ofSeq |> Async.RunSynchronously
    return! ctx.WriteJsonAsync costCenters
}

let addCostCenter (next: HttpFunc) (ctx: HttpContext) = task {
    use connection = new FileConnection(defaultFile)
    let connectionF () = Connection.SqliteConnection connection.Value
    let queries = Queries.CostCenter connectionF
    let! name = ctx.BindJsonAsync<string>()
    let! created = queries.New name
    match created with
    | Some costCenterId ->
        let costCenter =
            { CostCenter.Id = costCenterId
              Name = name }
        return! ctx.WriteJsonAsync costCenter
    | _ ->
        return! Response.internalError ctx ""
}

let delCostCenter (costCenterId: int) (next: HttpFunc) (ctx: HttpContext) = task {
    use connection = new FileConnection(defaultFile)
    let connectionF () = Connection.SqliteConnection connection.Value
    let queries = Queries.CostCenter connectionF
    let! _ = CostCenterId costCenterId |> queries.Delete
    // TODO fail if used (handled by constraints?)
    return! ctx.WriteJsonAsync ""
}
