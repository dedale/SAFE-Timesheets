module Program

open Expecto

let all =
    testList "All"
        [
            Shared.Tests.all
        ]

[<EntryPoint>]
let main _ =
    runTests defaultConfig all