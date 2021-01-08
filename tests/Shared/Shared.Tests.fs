module Shared.Tests

open Expecto

let dummyList = testList "List" [
    testCase "Test" <| fun _ ->
        Expect.isTrue true ""
]

let all = testList "all" [
    dummyList
]
