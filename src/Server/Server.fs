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

        get Route.user Handlers.User.getUsers
        post Route.user Handlers.User.addUser
        deletef "/api/user/%i" Handlers.User.delUser

        get Route.team Handlers.Team.getTeams
        post Route.team Handlers.Team.addTeam
        deletef "/api/team/%i" Handlers.Team.delTeam

        // https://blog.restcase.com/5-basic-rest-api-design-guidelines/
        // https://blog.octo.com/en/design-a-rest-api/#case_uri
        get Route.costCenterCamel Handlers.CostCenter.getCostCenters
        get Route.costCenterSpinal Handlers.CostCenter.getCostCenters
        post Route.costCenterCamel Handlers.CostCenter.addCostCenter
        post Route.costCenterSpinal Handlers.CostCenter.addCostCenter
        deletef "/api/cost_center/%i" Handlers.CostCenter.delCostCenter
        deletef "/api/cost-center/%i" Handlers.CostCenter.delCostCenter

        get Route.task Handlers.Task.getTasks
        post Route.task Handlers.Task.addTask
        deletef "/api/task/%i" Handlers.Task.delTask
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
