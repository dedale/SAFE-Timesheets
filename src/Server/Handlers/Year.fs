module Handlers.Year

open Shared

open Database

open FSharp.Data.Dapper
open FSharp.Control.Tasks.ContextInsensitive
open Giraffe
open Microsoft.AspNetCore.Http
open Saturn.ControllerHelpers

let getWeeks (userId: int, year: int) (next: HttpFunc) (ctx: HttpContext) = task {
    use connection = new FileConnection(defaultFile)
    let connectionF () = Connection.SqliteConnection connection.Value
    let weekDays = Queries.WeekDays connectionF
    match YearNumber.create year with
    | Ok y ->
        let weekDays =
            weekDays.GetWeeks (UserId userId) y
            |> Async.RunSynchronously
        let isWeekFull =
            weekDays
            |> List.map (fun wd -> wd.Week, wd.Days = WorkDays.max)
            |> Map.ofList
        // TODO use floats and factor with Home code
        let tryIsFull week =
            match isWeekFull.TryFind week with
            | Some b when b ->
                Some WeekStatus.Full
            | Some b when not b ->
                Week.age week |> WeekStatus.Incomplete |> Some
            | _ ->
                match Week.age week with
                | WeekAge.Future -> None
                | age -> Some (WeekStatus.Incomplete age)
        let monthWeeks = MonthWeeks.create year tryIsFull
        return! ctx.WriteJsonAsync monthWeeks
    | _ ->
        return! Response.internalError ctx ""
}
