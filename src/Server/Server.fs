module Server

open Giraffe
open Saturn

open Shared

let webApp =
    router {
        post Route.login Handlers.Login.login

        post Route.user Handlers.User.addUser
        deletef "/api/user/%i" Handlers.User.delUser

        post Route.team Handlers.Team.addTeam
        deletef "/api/team/%i" Handlers.Team.delTeam
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
