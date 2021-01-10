module Program

open Database

open Expecto
open FSharp.Data.Dapper

let all =
    testList "All"
        [
            FileSystem.Tests.all
            Database.Tests.all
            Server.Tests.all
        ]

[<EntryPoint>]
let main _ =
    OptionHandler.RegisterTypes()
    TypeHandlers.RegisterTypes()
    runTests defaultConfig all