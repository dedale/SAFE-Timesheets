module Program

open Expecto

let all =
    testList "All"
        [
            Client.Tests.all
        ]

[<EntryPoint>]
let main _ =
    runTests defaultConfig all