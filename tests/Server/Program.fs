module Program

open Expecto

let all =
    testList "All"
        [
            FileSystem.Tests.all
            Database.Tests.all
            Server.Tests.all
        ]

[<EntryPoint>]
let main _ =
    runTests defaultConfig all