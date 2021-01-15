module Handlers.Activity

open Shared

open Database

open FSharp.Data.Dapper
open FSharp.Control.Tasks.ContextInsensitive
open Giraffe
open Microsoft.AspNetCore.Http
open Saturn.ControllerHelpers

let getActivities (userId: int, year: int, number: int) (next: HttpFunc) (ctx: HttpContext) = task {
    use connection = new FileConnection(defaultFile)
    let connectionF () = Connection.SqliteConnection connection.Value
    let activity = Queries.Activity connectionF
    match Week.create number year with
    | Ok week ->
        let activities =
            activity.GetWeek (UserId userId) week
            |> Async.map List.ofSeq
            |> Async.RunSynchronously
        return! ctx.WriteJsonAsync activities
    | Error m ->
        return! Response.internalError ctx m
}

let addActivity (next: HttpFunc) (ctx: HttpContext) = task {
    let! newActivity = ctx.BindJsonAsync<NewActivity>()
    use connection = new FileConnection(defaultFile)
    let connectionF () = Connection.SqliteConnection connection.Value
    let activity = Queries.Activity connectionF
    let! created = activity.New newActivity.UserId newActivity.Date newActivity.TaskId newActivity.Days newActivity.Comment
    match created with
    | Some activityId ->
        let activity =
            { Activity.Id = activityId
              UserId = newActivity.UserId
              Date = newActivity.Date
              TaskId = newActivity.TaskId
              Days = newActivity.Days
              Comment = newActivity.Comment }
        return! ctx.WriteJsonAsync activity
    | _ ->
        return! Response.internalError ctx ""
}

let updateActivity (next: HttpFunc) (ctx: HttpContext) = task {
    let! updatedActivity = ctx.BindJsonAsync<Activity>()
    use connection = new FileConnection(defaultFile)
    let connectionF () = Connection.SqliteConnection connection.Value
    let activity = Queries.Activity connectionF
    let! _ = activity.Update updatedActivity
    return! ctx.WriteJsonAsync updatedActivity
}

let delActivity (activityId: int) (next: HttpFunc) (ctx: HttpContext) = task {
    use connection = new FileConnection(defaultFile)
    let connectionF () = Connection.SqliteConnection connection.Value
    let activity = Queries.Activity connectionF
    let! _ = ActivityId activityId |> activity.Delete
    // TODO fail if used (handled by constraints?)
    return! ctx.WriteJsonAsync ""
}
