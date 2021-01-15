module Server

open Giraffe
open Saturn

open Shared

open Database

open FSharp.Data.Dapper

OptionHandler.RegisterTypes()
TypeHandlers.RegisterTypes()
Queries.init() |> Async.RunSynchronously

let webApp =
    router {
        post Route.login Handlers.Login.login

        get Route.users Handlers.User.getUsers
        post Route.users Handlers.User.addUser
        deletef "/api/users/%i" Handlers.User.delUser

        //getf "/api/users/%i/teams" Handlers.User.getTeams

        get Route.teams Handlers.Team.getTeams
        post Route.teams Handlers.Team.addTeam
        deletef "/api/teams/%i" Handlers.Team.delTeam

        getf "/api/teams/%i/users" Handlers.Team.getTeamUsers
        postf "/api/teams/%i/tasks" Handlers.Team.addTeamTask
        getf "/api/teams/%i/tasks" Handlers.Team.getTeamTasks

        // https://blog.restcase.com/5-basic-rest-api-design-guidelines/
        // https://blog.octo.com/en/design-a-rest-api/#case_uri
        get Route.costCentersCamel Handlers.CostCenter.getCostCenters
        get Route.costCentersSpinal Handlers.CostCenter.getCostCenters
        post Route.costCentersCamel Handlers.CostCenter.addCostCenter
        post Route.costCentersSpinal Handlers.CostCenter.addCostCenter
        deletef "/api/cost_centers/%i" Handlers.CostCenter.delCostCenter
        deletef "/api/cost-centers/%i" Handlers.CostCenter.delCostCenter

        get Route.tasks Handlers.Task.getTasks
        post Route.tasks Handlers.Task.addTask
        deletef "/api/tasks/%i" Handlers.Task.delTask

        getf "/api/years/%i" Handlers.Year.getWeeks

        getf "/api/users/%i/activities/%i/%i" Handlers.Activity.getActivities
        post Route.activities Handlers.Activity.addActivity
        put Route.activities Handlers.Activity.updateActivity
        deletef "/api/activities/%i" Handlers.Activity.delActivity
    }

let app =
    application {
        url "http://0.0.0.0:8085"
        use_router webApp
        memory_cache
        use_static "public"
        use_json_serializer (Thoth.Json.Giraffe.ThothSerializer())
        use_gzip
    }

run app
