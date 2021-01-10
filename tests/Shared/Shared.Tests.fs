module Shared.Tests

open Shared

open Expecto
open System

let OkOrFail result =
    match result with
    | Ok d -> d
    | Error m -> failwith m

let date = testList "Date" [
    testCase "Create requires zero TimeOfDay" <| fun _ ->
        let error = SafeDate.create DateTime.UtcNow
        let expected = Error "TimeOfDay should be zero"
        Expect.equal error expected ""

    testCase "Create requires year >= 2007" <| fun _ ->
        let error = DateTime(2006, 12, 31) |> SafeDate.create
        let expected = Error "Year should be >= 2007"
        Expect.equal error expected ""

    testCase "Create recent date" <| fun _ ->
        let result = SafeDate.create DateTime.Today
        Expect.isOk result "Should create date"

    testCase "Min should be a Monday" <| fun _ ->
        let min = SafeDate.min
        Expect.equal ((SafeDate.value min).DayOfWeek) DayOfWeek.Monday "Minimum week should start on Monday"

    testCase "Min prev year should fail" <| fun _ ->
        let prev = (SafeDate.value SafeDate.min).Year - 1 |> YearNumber.create
        Expect.isError prev "Year before minimum date should not be allowed"
]

let week = testList "Week" [

    testList "Numbers" ([
        new DateTime(2007, 1, 1), 1
        new DateTime(2014, 12, 28), 52
        new DateTime(2014, 12, 29), 1
        new DateTime(2015, 1, 4), 1
        new DateTime(2015, 1, 5), 2
        new DateTime(2019, 1, 6), 1
        new DateTime(2019, 1, 7), 2
        new DateTime(2021, 1, 3), 53
        new DateTime(2021, 1, 4), 1
        new DateTime(2021, 1, 5), 1
        new DateTime(2021, 1, 10), 1
        new DateTime(2021, 1, 11), 2
    ] |> List.map (fun (date, number) ->
        test (date.ToString()) {
            let week = date |> SafeDate.create |> OkOrFail |> Week.ofDate
            Expect.equal week.Number number (sprintf "Bad week number for %A" date)
        })
    )

    testList "Start" ([
        new DateTime(2007, 1, 1), new DateTime(2007, 1, 1)
        new DateTime(2021, 1, 3), new DateTime(2020, 12, 28)
        new DateTime(2021, 1, 4), new DateTime(2021, 1, 4)
        new DateTime(2021, 1, 5), new DateTime(2021, 1, 4)
        new DateTime(2021, 1, 10), new DateTime(2021, 1, 4)
        new DateTime(2021, 1, 11), new DateTime(2021, 1, 11)
    ] |> List.map (fun (date, expected) ->
        test (date.ToString()) {
            let start = date |> SafeDate.create |> OkOrFail |> Week.start
            Expect.equal (SafeDate.value start) expected (sprintf "Bad start of week for %A" date)
        })
    )

    testList "Finish" ([
        new DateTime(2007, 1, 1), new DateTime(2007, 1, 5)
        new DateTime(2021, 1, 3), new DateTime(2021, 1, 1)
        new DateTime(2021, 1, 4), new DateTime(2021, 1, 8)
        new DateTime(2021, 1, 5), new DateTime(2021, 1, 8)
        new DateTime(2021, 1, 10), new DateTime(2021, 1, 8)
        new DateTime(2021, 1, 11), new DateTime(2021, 1, 15)
    ] |> List.map (fun (date, expected) ->
        test (date.ToString()) {
            let finish = date |> SafeDate.create |> OkOrFail |> Week.finish
            Expect.equal (SafeDate.value finish) expected (sprintf "Bad end of week for %A" date)
        })
    )

    testList "ofYear.OK" ([
        2007, 1
        2014, 52
        2014, 1
        2015, 1
        2019, 1
        2020, 53
        2021, 1
        2021, 2
    ] |> List.map (fun (year, number) ->
        test (sprintf "%d: %d" year number) { Expect.isOk (Week.create number year) "" })
    )

    testList "ofYear.Error" ([
        2007, -1
        2014, 53
        2020, 54
    ] |> List.map (fun (year, number) ->
        test (sprintf "%d: %d" year number) { Expect.isError (Week.create number year) "" })
    )

    testList "start" ([
        2007
        2014
        2015
        2019
        2020
        2021
    ] |> List.map (fun year ->
        test (year.ToString()) {
            let w = Week.first year
            Expect.equal w.Number 1 ""
            Expect.equal w.Year year ""
        })
    )

    testList "last" ([
        2007, 52
        2014, 52
        2015, 53
        2019, 52
        2020, 53
        2021, 52
    ] |> List.map (fun (year, last) ->
        test (year.ToString()) {
            let week = Week.last year
            Expect.equal week.Number last ""
            Expect.equal week.Year year ""
        })
    )

    testList "Range" [
        testCase "2021.1" <| fun _ ->
            let actual =
                match Week.create 1 2021 with
                | Ok w -> let m, f = (Week.range w) in SafeDate.value m, SafeDate.value f
                | Error m -> failtest m
            let expected = DateTime(2021, 1, 4), DateTime(2021, 1, 8)
            Expect.equal actual expected ""

        testCase "2020.53" <| fun _ ->
            let actual =
                match Week.create 53 2020 with
                | Ok w -> let m, f = (Week.range w) in SafeDate.value m, SafeDate.value f
                | Error m -> failtest m
            let expected = DateTime(2020, 12, 28), DateTime(2021, 1, 1)
            Expect.equal actual expected ""
    ]
]

let days = testList "Days" [
    testCase "Days should be >0" <| fun _ ->
        let error = WorkDays.create -1.
        let expected = Error "days should be > 0"
        Expect.equal error expected ""

    testCase "Days should be <= 5" <| fun _ ->
        let error = WorkDays.create 5.1
        let expected = Error "days should be <= 5"
        Expect.equal error expected ""

    testCase "Create OK" <| fun _ ->
        [ 0.1; 1.; 4.0; 5.0 ]
        |> List.iter (fun days -> 
            let result = WorkDays.create days
            Expect.isOk result "Should create date")
]

let all =
    testList "Domain"
        [
            date
            week
            days
        ]
