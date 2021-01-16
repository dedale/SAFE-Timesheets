﻿module Handlers.Year

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
        let monthWeeks = MonthWeeks.create year isWeekFull.TryFind
        return! ctx.WriteJsonAsync monthWeeks
    | Error m ->
        return! Response.internalError ctx ""
}
