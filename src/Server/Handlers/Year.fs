module Handlers.Year

open Shared

open FSharp.Control.Tasks.ContextInsensitive
open Giraffe
open Microsoft.AspNetCore.Http

let getWeeks (year: int) (next: HttpFunc) (ctx: HttpContext) = task {
    // TODO get week days from DB
    let tryIsFull (week: Week) =
        if week.Year <= 2020
        then Some true
        elif week.Year = 2021 then
            if week.Number <= 2
            then Some true
            elif week.Number = 3
            then Some false
            else None
        else None
    let monthWeeks = MonthWeeks.create year tryIsFull
    return! ctx.WriteJsonAsync monthWeeks
}
