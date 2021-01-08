module Program

open Expecto

let all =
    testList "All"
        [
            Server.Tests.all
        ]

[<EntryPoint>]
let main _ =
    runTests defaultConfig all