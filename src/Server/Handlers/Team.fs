module Handlers.Team

open Shared

open FSharp.Control.Tasks.ContextInsensitive
open Giraffe
open Microsoft.AspNetCore.Http

let addTeam (next: HttpFunc) (ctx: HttpContext) = task {
    // TODO db
    let! (teamName, managerId) = ctx.BindJsonAsync<string * UserId>()
    let id = TeamId (teamName.GetHashCode())
    let team =
        { Team.Id = id
          Name = teamName }
    return! ctx.WriteJsonAsync team
}

let delTeam (id: int) (next: HttpFunc) (ctx: HttpContext) = task {
    // TODO del from db
    // TODO fail if not empty
    return! ctx.WriteJsonAsync ""
}
