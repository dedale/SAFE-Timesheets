module Handlers.Activity

open Shared

open Database

open FSharp.Data.Dapper
open FSharp.Control.Tasks.ContextInsensitive
open Giraffe
open Microsoft.AspNetCore.Http
open Saturn.ControllerHelpers

open System.Threading.Tasks

let createActivity (queries: Queries.Activity) (newActivity: NewActivity) = task {
    let! created = queries.New newActivity.UserId newActivity.Date newActivity.TaskId newActivity.Days newActivity.Comment
    match created with
    | Some activityId ->
        return
            { Activity.Id = activityId
              UserId = newActivity.UserId
              Date = newActivity.Date
              TaskId = newActivity.TaskId
              Days = newActivity.Days
              Comment = newActivity.Comment } |> Some
    | _ ->
        return None
}

// TODO improve this?
let holidayId = TaskId 1

let addHoliday userId queries (date, name) = task {
    let activity =
        { NewActivity.UserId = userId
          Date = date
          TaskId = holidayId
          Days = WorkDays.one
          Comment = name
        }
    return! activity |> createActivity queries
}

let addHolidays userId (week: Week) activities = task {
    match activities with
    | x :: xs ->
        // When week not empty, holidays have been already added
        return activities
    | [] ->
        let (Monday, Friday) = Week.range week
        use connection = new FileConnection(defaultFile)
        let connectionF () = Connection.SqliteConnection connection.Value
        let queries = Queries.Activity connectionF
        let! created =
            PublicHolidays.ofYear week.Year
            |> List.filter (fun (date: SafeDate, _) -> date.Value >= Monday.Value && date.Value <= Friday.Value)
            |> List.map (addHoliday userId queries)
            |> Task.WhenAll
        let holidays =
            created
            |> List.ofArray
            |> List.choose id
        return holidays @ activities
}

let getActivities (user: int, year: int, number: int) (next: HttpFunc) (ctx: HttpContext) = task {
    use connection = new FileConnection(defaultFile)
    let connectionF () = Connection.SqliteConnection connection.Value
    let queries = Queries.Activity connectionF
    match Week.create number year with
    | Ok week ->
        let userId = UserId user
        let! activities =
            queries.GetWeek userId week
            |> Async.map List.ofSeq
            |> Async.RunSynchronously
            |> addHolidays userId week
        return! ctx.WriteJsonAsync activities
    | Error m ->
        return! Response.internalError ctx m
}

let addActivity (next: HttpFunc) (ctx: HttpContext) = task {
    let! newActivity = ctx.BindJsonAsync<NewActivity>()
    use connection = new FileConnection(defaultFile)
    let connectionF () = Connection.SqliteConnection connection.Value
    let queries = Queries.Activity connectionF
    let! created = createActivity queries newActivity
    match created with
    | Some activity ->
        return! ctx.WriteJsonAsync activity
    | _ ->
        return! Response.internalError ctx ""
}

let updateActivity (next: HttpFunc) (ctx: HttpContext) = task {
    let! updatedActivity = ctx.BindJsonAsync<Activity>()
    use connection = new FileConnection(defaultFile)
    let connectionF () = Connection.SqliteConnection connection.Value
    let queries = Queries.Activity connectionF
    let! _ = queries.Update updatedActivity
    return! ctx.WriteJsonAsync updatedActivity
}

let delActivity (activityId: int) (next: HttpFunc) (ctx: HttpContext) = task {
    use connection = new FileConnection(defaultFile)
    let connectionF () = Connection.SqliteConnection connection.Value
    let queries = Queries.Activity connectionF
    let! _ = ActivityId activityId |> queries.Delete
    // TODO fail if used (handled by constraints?)
    return! ctx.WriteJsonAsync ""
}
